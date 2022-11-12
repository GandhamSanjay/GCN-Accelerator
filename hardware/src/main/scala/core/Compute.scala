package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._
import gcn.core.util._
// /** Compute.
//  *
//  * Takes instructions from fetch module. Schedules computation between PEs.
//  * Arbitrates communication betwen PE and scratchpads.
//  */
class VRTableEntry()(implicit p: Parameters) extends Bundle{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val nRows = UInt(32.W)
  val isVRWithNextGroup = Bool()
}

class Compute(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val cr = p(AccKey).crParams
  val regBits = p(AccKey).crParams.regBits
  val io = IO(new Bundle {
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    val gbReadCmd = Output(new SPReadCmd)
    val gbReadData = Input(new SPReadData)
    val valid = Input(Bool())
    val done = Output(Bool())
  })
  val bankBlockSizeBytes = cp.bankBlockSize/8
  io.done := false.B

  // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  val dec = Module(new ComputeDecode)
  val vRTable = SyncReadMem(cp.nGroups, new VRTableEntry)
  val groupArray = for(i <- 0 until cp.nGroups) yield {
    Module(new Group)
  }


// state machine
  val sIdle :: sDataMoveRow :: sDataMoveRowGroup :: sDataMoveCol :: sDataMoveVal :: sDataMoveDen ::sCompute :: sWait :: sDone :: Nil = Enum(9)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val inst = RegEnable(inst_q.io.deq.bits, start)
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && io.valid

// RowPtrSplitting
  val groupSel = RegInit(0.U(cp.nGroups.W))
  val groupEnd = groupSel === (cp.nGroups - 1).U
  val nNonZeroPerGroup =  dec.io.colSize >> log2Ceil(cp.nGroups)
  val gbAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))
  val gbRdata = io.gbReadData.data
  val rowPtrDataBlock = for(i <- 0 until (cp.bankBlockSize/cp.blockSize))yield{
    gbRdata((((i+1)*cp.blockSize) -1), i*cp.blockSize)
  }

  val rowPtrEnd = rowPtrDataBlock.map(_ >= ((groupSel+1.U) << Log2(nNonZeroPerGroup)))
  val rowPtrFin = rowPtrEnd.reduce(_||_)
  val rowPtrDataPrev = for(i <- 0 until (cp.bankBlockSize/cp.blockSize))yield{
    RegEnable(rowPtrDataBlock(i), rowPtrFin)
  }
  val rowPtrEndPrev = rowPtrDataPrev.map(_ >= ((groupSel+1.U) << Log2(nNonZeroPerGroup)))
  val rowPtrFinPrev = rowPtrEndPrev.reduce(_||_)
  val rowPtrPrevWriteMask = rowPtrEndPrev.map(!_).map(_.asUInt)
  val rowPtrPrevWriteEn =  rowPtrPrevWriteMask.reduce(_.asBool||_.asBool)

  val rowPtrAddr = RegInit(bankBlockSizeBytes.U(32.W))
  val rowPtrGroupFin = (groupSel === (cp.nGroups - 1).U)
  val rowPtrReadAddr = Mux(start, dec.io.sramPtr, rowPtrAddr) 
  val rowPtrWriteMask = rowPtrEnd.map(!_).map(_.asUInt)
  val rowPtrWriteEn =  rowPtrWriteMask.reduce(_.asBool||_.asBool)
  val rowPtrWriteAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))


  when((state === sDataMoveRow)){
    when(rowPtrFin){
      rowPtrWriteAddr := 0.U
    }.otherwise{
      rowPtrWriteAddr := rowPtrWriteAddr + bankBlockSizeBytes.U
    }
  }.elsewhen(state === sDataMoveRowGroup){
    when(rowPtrFinPrev){
      rowPtrWriteAddr := 0.U
    }.otherwise{
      rowPtrWriteAddr := rowPtrWriteAddr + bankBlockSizeBytes.U
    }
  }

  // Col Splitting
  val colReadAddr = RegInit(0.U(32.W))
  val colWriteAddr = RegInit(0.U(32.W))
  val colReadBlockNum = RegInit(cp.nColInDense.U(32.W))
  val colFin = (colReadBlockNum >= ((groupSel + 1.U) << Log2(nNonZeroPerGroup)))
  when(state === sDataMoveCol){
    colReadBlockNum := colReadBlockNum + cp.nColInDense.U
  }
  when((state === sDataMoveCol)){
    when(colFin){
      colWriteAddr := 0.U
    }.otherwise{
      colWriteAddr := colWriteAddr + bankBlockSizeBytes.U
    }
  }

  // Val Splitting
  val valReadAddr = RegInit(0.U(32.W))
  val valWriteAddr = RegInit(0.U(32.W))
  val valReadBlockNum = RegInit(cp.nColInDense.U(32.W))
  val valFin = (valReadBlockNum >= ((groupSel + 1.U) << Log2(nNonZeroPerGroup)))
  when(state === sDataMoveVal){
    valReadBlockNum := valReadBlockNum + cp.nColInDense.U
  }
  when((state === sDataMoveVal)){
    when(valFin){
      valWriteAddr := 0.U
    }.otherwise{
      valWriteAddr := valWriteAddr + bankBlockSizeBytes.U
    }
  }

  // Den Splitting
  val denReadAddr = RegInit(0.U(32.W))
  val denWriteAddr = RegInit(0.U(32.W))
  val denReadBlockNum = RegInit(cp.nColInDense.U(32.W))
  val denFin = (denReadBlockNum >= dec.io.denSize)
  when(state === sDataMoveDen){
    denReadBlockNum := denReadBlockNum + cp.nColInDense.U
  }
  when((state === sDataMoveVal)){
    when(valFin){
      valWriteAddr := 0.U
    }.otherwise{
      valWriteAddr := valWriteAddr + bankBlockSizeBytes.U
    }
  }

// group io
  for(i <- 0 until cp.nGroups){
    groupArray(i).io.spWrite.bits.spSel :=     
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveVal -> 0.U,
        sDataMoveRowGroup -> 2.U,
        sDataMoveRow -> 2.U,
        sDataMoveCol -> 3.U,
        sDataMoveDen -> 1.U
      ))
    groupArray(i).io.spWrite.bits.spWriteCmd.addr :=
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveRowGroup -> rowPtrWriteAddr,
        sDataMoveRow -> rowPtrWriteAddr,
        sDataMoveCol -> colWriteAddr,
        sDataMoveVal -> valWriteAddr,
        sDataMoveDen -> denWriteAddr
      ))
    groupArray(i).io.spWrite.bits.spWriteCmd.data := 
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveRowGroup -> rowPtrDataPrev.reverse.reduce(Cat(_,_)),
        sDataMoveRow -> rowPtrDataBlock.reverse.reduce(Cat(_,_)),
        sDataMoveCol -> io.gbReadData.data,
        sDataMoveVal -> io.gbReadData.data,
        sDataMoveDen -> io.gbReadData.data
      ))
    groupArray(i).io.spWrite.valid := Mux(state === sDataMoveDen, true.B, 
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveRowGroup -> rowPtrPrevWriteEn,
        sDataMoveRow -> rowPtrWriteEn,
        sDataMoveCol -> true.B,
        sDataMoveVal -> true.B
      )).asBool  && (groupSel === i.U))
    groupArray(i).io.mask:= 
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveRowGroup -> rowPtrPrevWriteMask.reverse.reduce(Cat(_,_)),
        sDataMoveRow -> rowPtrWriteMask.reverse.reduce(Cat(_,_)),
        sDataMoveCol -> ((cp.nColInDense.U << 1.U) - 1.U),
        sDataMoveVal -> ((cp.nColInDense.U << 1.U) - 1.U),
        sDataMoveDen -> ((cp.nColInDense.U << 1.U) - 1.U)
      ))
  }

// group select
  when(((state === sDataMoveRow) && rowPtrFin)||((state === sDataMoveRowGroup) && rowPtrFinPrev)||((state === sDataMoveCol) && colFin)||((state === sDataMoveVal) && valFin)){
    when(groupEnd){
      groupSel := 0.U
    }.otherwise{
      groupSel := groupSel + 1.U
    }
  }
  
  io.gbReadCmd.addr := MuxLookup(true.B,
      rowPtrReadAddr, // default
      Array(
        ((state === sIdle) && start)-> rowPtrReadAddr,
        ((state === sDataMoveRow) && (!(rowPtrFin && groupEnd))) -> rowPtrReadAddr,
        ((state === sDataMoveRowGroup) && (!(rowPtrFinPrev && groupEnd))) -> rowPtrReadAddr,
        ((state === sDataMoveRow) && (rowPtrFin && groupEnd)) -> colReadAddr,
        ((state === sDataMoveRowGroup) && (rowPtrFinPrev && groupEnd)) -> colReadAddr,
        ((state === sDataMoveCol) && !(colFin && groupEnd)) -> colReadAddr,
        ((state === sDataMoveCol) && (colFin && groupEnd)) -> valReadAddr,
        ((state === sDataMoveVal) && !(valFin && groupEnd)) -> valReadAddr,
        ((state === sDataMoveVal) && (valFin && groupEnd)) -> denReadAddr,
        ((state === sDataMoveDen)) -> denReadAddr
      ))

//state machine
  switch(state){
    is(sIdle){
      when(start){
        state := sDataMoveRow
        rowPtrAddr := dec.io.sramPtr + bankBlockSizeBytes.U
        colReadAddr := dec.io.sramCol
        valReadAddr := dec.io.sramVal
        denReadAddr := dec.io.sramDen
      }
    }
    is(sDataMoveRow){
      when(rowPtrFin){
        when(groupEnd){
          state := sDataMoveCol
          colReadAddr := colReadAddr + bankBlockSizeBytes.U
        }.otherwise{
          state := sDataMoveRowGroup
        }
      }.otherwise{
        rowPtrAddr := rowPtrAddr + bankBlockSizeBytes.U
      }
    }
    is(sDataMoveRowGroup){
      when(rowPtrFinPrev){
         when(groupEnd){
          state := sDataMoveRowGroup
          colReadAddr := colReadAddr + bankBlockSizeBytes.U
         }
      }.otherwise{
        state := sDataMoveRow
      }
    }
    is(sDataMoveCol){
      when(colFin){
        when(groupEnd){
          state := sDataMoveVal
          valReadAddr := valReadAddr + bankBlockSizeBytes.U
        }
      }.otherwise{
        colReadAddr := colReadAddr + bankBlockSizeBytes.U
      }
    }
    is(sDataMoveVal){
      when(valFin){
        when(groupEnd){
          state := sDataMoveDen
          denReadAddr := denReadAddr + bankBlockSizeBytes.U
        }
      }.otherwise{
        valReadAddr := valReadAddr + bankBlockSizeBytes.U
      }
    }
    is(sDataMoveDen){
      denReadAddr := denReadAddr + bankBlockSizeBytes.U
      when(denFin){
        state := sCompute
      }
    }
  }









//   val gbInc = WireDefault(0.U(M_SRAM_OFFSET_BITS.W))
//   val gbRdata = io.gbReadData.data
  
//   val nNonZeroPerGroup =  dec.io.colSize >> log2Ceil(cp.nGroups)

//   val blocksPerBank = cp.bankBlockSize/cp.blockSize
//   // state machine
//   val sIdle :: sDataMoveRow :: sDataMoveCol :: sDataMoveVal :: sDataMoveDen :: sCompute :: sWait :: sDone :: Nil = Enum(8)
//   val state = RegInit(sIdle)
//   val isDataMove  = (state === sDataMoveRow) ||(state === sDataMoveCol) ||(state === sDataMoveVal) ||(state === sDataMoveDen)
//   val start = inst_q.io.deq.fire
//   val ctr = RegInit(0.U(5.W))
//   val inst = RegEnable(inst_q.io.deq.bits, start)
//   val blockSizeBytes = (cp.blockSize/8)
//   val bankBlockSizeBytes = (cp.bankBlockSize/8)
//   // val sramSize = cp.scratchValSize/cp.blockSize
//   val done = RegInit(false.B)
//   io.done := done
//   dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)
//   val groupSel = RegInit(0.U(cp.nGroups.W))

//   // RowPtr Splitting
//   val rowPtrBase = dec.io.sramPtr
//   val rowPtrDataBlock = for(i <- 0 until (cp.bankBlockSize/cp.blockSize))yield{
//     gbRdata((((i+1)*cp.blockSize) -1), i*cp.blockSize)
//   }
//   val rowPtrEnd = rowPtrDataBlock.map(_ >= nNonZeroPerGroup)
//   val rowPtrFin = rowPtrEnd.reduce(_||_)
//   val rowPtrInc = RegInit(0.U(32.W))
//   val rowPtrGroupFin = (groupSel === (cp.nGroups - 1).U)
//   when(start || (state === sDataMoveRow)){
//     rowPtrInc := rowPtrInc + bankBlockSizeBytes.U
//   }


  
//   val rowPtrWriteMask = rowPtrEnd.map(!_)
//   val rowPtrAddr = rowPtrBase + rowPtrInc
//   val rowPtrWriteEn =  rowPtrWriteMask.reduce(_||_)
//   val rowPtrWriteAddr = RegNext(rowPtrAddr)
//     // ColIdx Splitting
//   val colIdxBase = dec.io.sramCol
//   val colIdxInc = RegInit(bankBlockSizeBytes.U(32.W))
//   val colIdxAddr = colIdxBase + colIdxInc
//     when((state === sDataMoveRow) && rowPtrGroupFin || (state === sDataMoveCol)){
//     colIdxInc := colIdxInc + bankBlockSizeBytes.U
//   }

//   val colTransferLen = nNonZeroPerGroup
//   val colTransferLenRem = Reg(chiselTypeOf(nNonZeroPerGroup))
//   val colIdxFin = (colTransferLenRem === 0.U)
//   val colIdxGroupFin =  (groupSel === (cp.nGroups - 1).U)
//   when(state === sDataMoveRow){
//     colTransferLenRem := colTransferLen - blocksPerBank.U
//   }.elsewhen(state === sDataMoveCol){
//     when(colIdxFin){
//       colTransferLenRem := colTransferLen - blocksPerBank.U
//     }.otherwise{
//       colTransferLenRem := colTransferLenRem - blocksPerBank.U
//     }  
//   }

//   // Val Splitting
//   val valBase = dec.io.sramVal
//   val valInc = RegInit(bankBlockSizeBytes.U(32.W))
//   val valAddr = valBase + valInc
//   when((state === sDataMoveCol) && colIdxGroupFin || (state === sDataMoveVal)){
//     valInc := valInc + bankBlockSizeBytes.U
//   }
//   val valTransferLen = nNonZeroPerGroup
//   val valTransferLenRem = Reg(chiselTypeOf(nNonZeroPerGroup))
//   val valFin = (valTransferLenRem === 0.U)
//   val valGroupFin =  (groupSel === (cp.nGroups - 1).U)
//   when(state === sDataMoveCol){
//     valTransferLenRem := valTransferLen - blocksPerBank.U
//   }.elsewhen(state === sDataMoveVal){
//     when(valFin){
//       valTransferLenRem := valTransferLen - blocksPerBank.U
//     }.otherwise{
//       valTransferLenRem := valTransferLenRem - blocksPerBank.U
//     }  
//   }
  
//   io.gbReadCmd.addr :=rowPtrAddr
//   when(rowPtrGroupFin || colIdxGroupFin){
//     groupSel := 0.U
//   }.elsewhen(rowPtrFin || colIdxFin){
//     groupSel := groupSel + 1.U 
//   }
//   for(i <- 0 until cp.nGroups){
//     groupArray(i).io.spWrite.bits.spSel :=
//       MuxLookup(state,
//         2.U, // default
//         Array(
//           sDataMoveVal -> 0.U,
//           sDataMoveDen -> 1.U,
//           sDataMoveRow -> 2.U,
//           sDataMoveCol -> 3.U
//         ))
//     groupArray(i).io.spWrite.bits.spWriteCmd.addr :=
//       MuxLookup(state,
//         rowPtrWriteAddr, // default
//         Array(
//           sDataMoveVal -> 0.U,
//           sDataMoveDen -> 1.U,
//           sDataMoveRow -> rowPtrWriteAddr,
//           sDataMoveCol -> 3.U
//         ))
//     groupArray(i).io.spWrite.bits.spWriteCmd.data := io.gbReadData.data
//     groupArray(i).io.spWrite.valid := (groupSel === i.U) && isDataMove
//   }

//   // control
//   switch(state) {
//     is(sIdle) {
//       done := false.B
//       when(start){
//         state := sDataMoveRow
//       }
//     }
//     is(sDataMoveRow){
//       when(rowPtrFin){
//         when(rowPtrGroupFin){
//           state := sIdle
//         }
//       }
//     }
//     is(sDataMoveCol){
//       when(colIdxFin){
//         when(colIdxGroupFin){
//           state := sDataMoveVal
//         }
//       }
//     }
//     is(sDataMoveVal){
//       when(valFin){
//         when(valGroupFin){
//           state := sDataMoveDen
//         }
//       }
//     }
//     is(sWait){
//       state := sIdle
//   }
// }

//   // instructions
//   inst_q.io.enq <> io.inst
//   inst_q.io.deq.ready := (state === sIdle) && io.valid
}
