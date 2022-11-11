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

  // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  val dec = Module(new ComputeDecode)
  val vRTable = SyncReadMem(cp.nGroups, new VRTableEntry)
  val groupArray = for(i <- 0 until cp.nGroups) yield {
    Module(new Group)
  }
  val gbAddr = RegInit(0.U(M_SRAM_OFFSET_BITS.W))
  val gbInc = WireDefault(0.U(M_SRAM_OFFSET_BITS.W))
  val gbRdata = io.gbReadData.data
  
  val nNonZeroPerGroup =  dec.io.colSize >> log2Ceil(cp.nGroups)

  val blocksPerBank = cp.bankBlockSize/cp.blockSize
  // state machine
  val sIdle :: sDataMoveRow :: sDataMoveCol :: sDataMoveVal :: sDataMoveDen :: sCompute :: sWait :: sDone :: Nil = Enum(8)
  val state = RegInit(sIdle)
  val isDataMove  = (state === sDataMoveRow) ||(state === sDataMoveCol) ||(state === sDataMoveVal) ||(state === sDataMoveDen)
  val start = inst_q.io.deq.fire
  val ctr = RegInit(0.U(5.W))
  val inst = RegEnable(inst_q.io.deq.bits, start)
  val blockSizeBytes = (cp.blockSize/8)
  val bankBlockSizeBytes = (cp.bankBlockSize/8)
  // val sramSize = cp.scratchValSize/cp.blockSize
  val done = RegInit(false.B)
  io.done := done
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)
  val groupSel = RegInit(0.U(log2Ceil(cp.nGroups).W))

  // RowPtr Splitting
  val rowPtrBase = dec.io.sramPtr
  val rowPtrDataBlock = for(i <- 0 until (cp.bankBlockSize/cp.blockSize))yield{
    gbRdata((((i+1)*cp.blockSize) -1), i*cp.blockSize)
  }
  val rowPtrEnd = rowPtrDataBlock.map(_ >= (groupSel << Log2(nNonZeroPerGroup)))
  val rowPtrFin = rowPtrEnd.reduce(_||_)
  val rowPtrInc = RegInit(bankBlockSizeBytes.U(32.W))
  val rowPtrGroupFin = (groupSel === (cp.nGroups - 1).U)
  when(start || (state === sDataMoveRow)){
    rowPtrInc := rowPtrInc + bankBlockSizeBytes.U
  }


  
  val rowPtrWriteMask = rowPtrEnd.map(!_)
  val rowPtrAddr = rowPtrBase + rowPtrInc
  val rowPtrWriteEn =  rowPtrWriteMask.reduce(_||_)
  val rowPtrWriteAddr = RegNext(rowPtrAddr)
    // ColIdx Splitting
  val colIdxBase = dec.io.sramCol
  val colIdxInc = RegInit(bankBlockSizeBytes.U(32.W))
  val colIdxAddr = colIdxBase + colIdxInc
    when((state === sDataMoveRow) && rowPtrGroupFin || (state === sDataMoveCol)){
    colIdxInc := colIdxInc + bankBlockSizeBytes.U
  }

  val colTransferLen = nNonZeroPerGroup
  val colTransferLenRem = Reg(chiselTypeOf(nNonZeroPerGroup))
  val colIdxFin = (colTransferLenRem === 0.U)
  val colIdxGroupFin =  (groupSel === (cp.nGroups - 1).U)
  when(state === sDataMoveRow){
    colTransferLenRem := colTransferLen - blocksPerBank.U
  }.elsewhen(state === sDataMoveCol){
    when(colIdxFin){
      colTransferLenRem := colTransferLen - blocksPerBank.U
    }.otherwise{
      colTransferLenRem := colTransferLenRem - blocksPerBank.U
    }  
  }

  // Val Splitting
  val valBase = dec.io.sramVal
  val valInc = RegInit(bankBlockSizeBytes.U(32.W))
  val valAddr = valBase + valInc
  when((state === sDataMoveCol) && colIdxGroupFin || (state === sDataMoveVal)){
    valInc := valInc + bankBlockSizeBytes.U
  }
  val valTransferLen = nNonZeroPerGroup
  val valTransferLenRem = Reg(chiselTypeOf(nNonZeroPerGroup))
  val valFin = (valTransferLenRem === 0.U)
  val valGroupFin =  (groupSel === (cp.nGroups - 1).U)
  when(state === sDataMoveCol){
    valTransferLenRem := valTransferLen - blocksPerBank.U
  }.elsewhen(state === sDataMoveVal){
    when(valFin){
      valTransferLenRem := valTransferLen - blocksPerBank.U
    }.otherwise{
      valTransferLenRem := valTransferLenRem - blocksPerBank.U
    }  
  }
  
  io.gbReadCmd.addr := Mux(start, rowPtrAddr , gbAddr)
  when(rowPtrGroupFin || colIdxGroupFin){
    groupSel := 0.U
  }.elsewhen(rowPtrFin || colIdxFin){
    groupSel := groupSel + 1.U 
  }
  for(i <- 0 until cp.nGroups){
    groupArray(i).io.spWrite.bits.spSel :=
      MuxLookup(state,
        2.U, // default
        Array(
          sDataMoveVal -> 0.U,
          sDataMoveDen -> 1.U,
          sDataMoveRow -> 2.U,
          sDataMoveCol -> 3.U
        ))
    groupArray(i).io.spWrite.bits.spWriteCmd.addr :=
      MuxLookup(state,
        rowPtrWriteAddr, // default
        Array(
          sDataMoveVal -> 0.U,
          sDataMoveDen -> 1.U,
          sDataMoveRow -> rowPtrWriteAddr,
          sDataMoveCol -> 3.U
        ))
    groupArray(i).io.spWrite.bits.spWriteCmd.data := io.gbReadData.data
    groupArray(i).io.spWrite.valid := (groupSel === i.U) && isDataMove
  }

  // control
  switch(state) {
    is(sIdle) {
      done := false.B
      gbAddr := rowPtrAddr
      when(start){
        state := sDataMoveRow
      }
    }
    is(sDataMoveRow){
      when(rowPtrFin){
        when(rowPtrGroupFin){
          gbAddr := colIdxAddr
          state := sDataMoveCol
        }
      }
    }
    is(sDataMoveCol){
      when(colIdxFin){
        when(colIdxGroupFin){
          gbAddr := rowPtrAddr
          state := sDataMoveVal
        }
      }
    }
    is(sDataMoveVal){
      when(valFin){
        when(valGroupFin){
          gbAddr := valAddr
          state := sDataMoveDen
        }
      }
    }
    is(sWait){
      state := sIdle
  }
}

  // instructions
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && io.valid

//   // Compute execution time
//   when(done){
//     computeTime := 0.U
//   }.elsewhen(start || computeTime =/= 0.U){
//     computeTime := computeTime + 1.U
//   }

//   io.ecnt(0) := computeTime
}
