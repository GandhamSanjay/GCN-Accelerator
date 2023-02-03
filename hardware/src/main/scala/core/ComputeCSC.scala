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
class VCTableEntry()(implicit p: Parameters) extends Bundle{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val nCols = UInt(32.W)
  val isVCWithPrevGroup = Bool()
}
class VCTableEntryWithGroup()(implicit p: Parameters) extends Bundle{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val VCTableEntry = new VCTableEntry
  val group = UInt(cp.nGroups.W)
}

class ComputeCSC(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val cr = p(AccKey).crParams
  val regBits = p(AccKey).crParams.regBits
  val io = IO(new Bundle {
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    val gbReadCmd = Output(new SPReadCmd)
    val gbReadData = Input(new SPReadData(scratchType = "Global"))
    val spOutWrite = Decoupled(new SPWriteCmd)
    val valid = Input(Bool())
    val done = Output(Bool())
  })
  val bankBlockSizeBytes = cp.bankBlockSize/8
  val denseLoaded = RegInit(false.B)
  val computeTimeOut = RegInit(0.U(32.W))
  val computeTimer = RegInit(0.U(32.W))
  val computeSkipCount = RegInit(0.U(32.W))
  dontTouch(computeSkipCount)

  // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  val dec = Module(new ComputeDecode)
  val vCTable = SyncReadMem(cp.nGroups, new VCTableEntry)
  val vCTableReadGroup_q = RegInit(0.U(log2Ceil(cp.nGroups).W))
  val vCTableReadGroup = Wire(chiselTypeOf(vCTableReadGroup_q))
  val vCTableReadData = vCTable.read(vCTableReadGroup, true.B)
  val groupArray = for(i <- 0 until cp.nGroups) yield {
    Module(new GroupCSC(groupID = i))
  }

val vcArbiter = Module(new Arbiter(new VCTableEntryWithGroup, cp.nGroups))
vcArbiter.io.out.ready := true.B
when(vcArbiter.io.out.valid){
  vCTable.write(vcArbiter.io.out.bits.group,vcArbiter.io.out.bits.VCTableEntry)
}


// state machine
  val sIdle :: sDataMoveCol :: sDataMoveRow :: sDataMoveVal :: sDataMoveDen :: sCompute :: sCombineGroup :: sCombine :: sDone :: Nil = Enum(9)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val inst = RegEnable(inst_q.io.deq.bits, start)
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && io.valid


  val groupSel = RegInit(0.U(cp.nGroups.W))
  val groupEnd = groupSel === (cp.nGroups - 1).U
  val nNonZeroPrevTotal = RegInit(0.U(32.W))
  
  val nNonZeroPerGroup =  dec.io.rowSize >> log2Ceil(cp.nGroups)
  val gbAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))
  val gbRdata = io.gbReadData.data

  // Col Splitting
  val colPtrFin = Wire(Bool())
  val colPtrDataBlock = for(i <- 0 until (cp.bankBlockSize/cp.blockSize))yield{
    gbRdata((((i+1)*cp.blockSize) -1), i*cp.blockSize)
  }
  val colPtrAddr = RegInit(0.U(32.W))
  val colPtrIdxInBlock = colPtrAddr(log2Ceil(bankBlockSizeBytes)-1,log2Ceil(cp.blockSize/8))
  val colPtrData = MuxTree(colPtrIdxInBlock, colPtrDataBlock)
  val colPtrReadAddr = Mux(start, dec.io.sramPtr, Mux(colPtrFin, colPtrAddr, colPtrAddr + (cp.blockSize/8).U)) 
  colPtrFin := colPtrData >= (nNonZeroPrevTotal + (( groupSel + 1.U) << Log2(nNonZeroPerGroup)))
  val colPtrWriteAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))
  when(state === sDataMoveCol){
    when(colPtrFin){
      colPtrWriteAddr := 0.U
    }.otherwise{
      colPtrWriteAddr := colPtrWriteAddr + (cp.blockSize/8).U
    }
  }
  // val colPtrWriteMask = UIntToOH(colPtrIdxInBlock)
  val colPtrWriteEn = !colPtrFin && (state === sDataMoveCol)
  val nColWritten_q = RegInit(0.U(32.W))
  val nColWritten =  nColWritten_q + !colPtrFin
  val nColWrittenValid = Wire(Bool())
  
  // Row Splitting
  val rowReadAddr = RegInit(0.U(32.W))
  val rowWriteAddr = RegInit(0.U(32.W))
  val rowReadBlockNum = RegInit(cp.nRowInDense.U(32.W))
  val rowFin = (rowReadBlockNum >= ((groupSel + 1.U) << Log2(nNonZeroPerGroup)))
  when(state === sDataMoveRow){
    rowReadBlockNum := rowReadBlockNum + cp.nRowInDense.U
  }.otherwise{
    rowReadBlockNum := cp.nRowInDense.U
  }
  when((state === sDataMoveRow)){
    when(rowFin){
      rowWriteAddr := 0.U
    }.otherwise{
      rowWriteAddr := rowWriteAddr + bankBlockSizeBytes.U
    }
  }

  // Val Splitting
  val valReadAddr = RegInit(0.U(32.W))
  val valWriteAddr = RegInit(0.U(32.W))
  val valReadBlockNum = RegInit(cp.nRowInDense.U(32.W))
  val valFin = (valReadBlockNum >= ((groupSel + 1.U) << Log2(nNonZeroPerGroup)))
  when(state === sDataMoveVal){
    valReadBlockNum := valReadBlockNum + cp.nRowInDense.U
  }.otherwise{
    valReadBlockNum := cp.nRowInDense.U
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
  }.otherwise{
    denReadBlockNum := cp.nColInDense.U
  }
  when((state === sDataMoveDen)){
    when(denFin){
      denWriteAddr := 0.U
    }.otherwise{
      denWriteAddr := denWriteAddr + bankBlockSizeBytes.U
    }
  }

// group select
  when((((state === sDataMoveCol) && colPtrFin)||(state === sDataMoveRow) && rowFin)||((state === sDataMoveVal) && valFin)){
    when(groupEnd){
      groupSel := 0.U
      nColWritten_q := 0.U
    }.otherwise{
      groupSel := groupSel + 1.U
      nColWritten_q := 0.U
    }
  }.elsewhen((state === sDataMoveCol)){
    nColWritten_q := nColWritten
  }
  nColWrittenValid := ((state === sDataMoveCol) && colPtrFin)
  
  io.gbReadCmd.addr := MuxLookup(true.B,
      colPtrReadAddr, // default
      Array(
        ((state === sIdle) && start)-> colPtrReadAddr,
        ((state === sDataMoveCol) && (!(colPtrFin && groupEnd))) -> colPtrReadAddr,
        ((state === sDataMoveCol) && (colPtrFin && groupEnd)) -> rowReadAddr,
        ((state === sDataMoveRow) && !(rowFin && groupEnd)) -> rowReadAddr,
        ((state === sDataMoveRow) && (rowFin && groupEnd)) -> valReadAddr,
        ((state === sDataMoveVal) && !(valFin && groupEnd)) -> valReadAddr,
        ((state === sDataMoveVal) && (valFin && groupEnd)) -> denReadAddr,
        ((state === sDataMoveDen)) -> denReadAddr
      ))


// Partial outputs aggregate 
val outColCount_q = RegInit(0.U(32.W))
val aggDone = vCTableReadGroup_q === (cp.nGroups - 1).U
val currColInGroup_q = RegInit(0.U(32.W))
val currColInGroup = Wire(chiselTypeOf(currColInGroup_q))
val nColInGroup = vCTableReadData.nCols
val isPR = dec.io.prStart && (outColCount_q === 0.U)
val isVC = vCTableReadData.isVCWithPrevGroup && !isPR
val groupOutAddr = currColInGroup << log2Ceil((cp.blockSize * cp.nRowInDense)/8)
val groupOutData = Wire(chiselTypeOf(groupArray(0).io.outReadData))
val groupOutDataPrev = RegEnable(groupOutData, state === sCombine)
val aggWithPrevGroup = ((currColInGroup_q === 0.U) && isVC)
val outDataAgg = groupOutData.map(_.data).zip(groupOutDataPrev.map(_.data)).map{case(d,dP) => d+dP}.reverse.reduce{Cat(_,_)}
val prData_q = Reg(chiselTypeOf(outDataAgg))
val prSplitData = for(i <- 0 until cp.nColInDense)yield{
  prData_q(((i+1)*cp.blockSize) -1, i*cp.blockSize)
}
val prStartCol = (state === sCombine) && (currColInGroup_q === 0.U) && (vCTableReadGroup_q === 0.U)
val prColAgg = dec.io.prStart && prStartCol
val outDataPrAgg = groupOutData.map(_.data).zip(prSplitData).map{case(d,dP) => d+dP}.reverse.reduce{Cat(_,_)}
val outDataNoAgg = groupOutData.map(_.data).reverse.reduce(Cat(_,_))
val outData = Mux(aggWithPrevGroup, outDataAgg, Mux(prColAgg, outDataPrAgg, outDataNoAgg))

val outvCCount_q = RegInit(0.U(32.W))
val outvCCount = Mux(state === sCombine && (RegNext(state)===sCombineGroup), outvCCount_q + isVC.asUInt, outvCCount_q)
when(state === sCombine && (RegNext(state)===sCombineGroup)){
  outvCCount_q := outvCCount
}.elsewhen(state === sIdle){
  outvCCount_q := 0.U
}
when(state === sCombine){
  outColCount_q := outColCount_q + 1.U
}.elsewhen(state === sIdle){
  outColCount_q := 0.U
}
val outWriteAddr = (outColCount_q - outvCCount) << log2Ceil((cp.blockSize * cp.nRowInDense)/8)
currColInGroup := Mux(state === sCombine, currColInGroup_q + 1.U, currColInGroup_q)
val outWriteEn = state === sCombine
when((state === sCompute)){
  vCTableReadGroup_q := 0.U
}.elsewhen(((state === sCombine) || (state === sCombineGroup)) && (currColInGroup === nColInGroup)){
  vCTableReadGroup_q := vCTableReadGroup_q + 1.U
}

when(state === sCombine){
  when(currColInGroup === (nColInGroup)){
    currColInGroup_q := 0.U
  }.otherwise{
    currColInGroup_q := currColInGroup
  }
}
vCTableReadGroup := Mux(((state === sCombine) || (state === sCombineGroup)) && (currColInGroup === nColInGroup),vCTableReadGroup_q + 1.U,vCTableReadGroup_q)

io.spOutWrite.bits.addr := outWriteAddr
io.spOutWrite.bits.data := outData
io.spOutWrite.valid := outWriteEn

// pr partial sum io
val prEndCol = (state === sCombine) && (currColInGroup_q === (nColInGroup - 1.U)) && (vCTableReadGroup_q === (cp.nGroups - 1).U)
val prColWrite = dec.io.prEnd && prEndCol
val prData = outData
when(prColWrite){
  prData_q := prData
}



// group io
  for(i <- 0 until cp.nGroups){
    groupArray(i).io.outReadCmd.map(_.addr := groupOutAddr)
    groupOutData := MuxTree(vCTableReadGroup_q, groupArray.map(_.io.outReadData))
    groupArray(i).io.nColPtrInGroup.bits := nColWritten
    groupArray(i).io.nColPtrInGroup.valid := (nColWrittenValid && groupSel === i.U)
    vcArbiter.io.in(i).bits.VCTableEntry := groupArray(i).io.vcEntry.bits
    vcArbiter.io.in(i).bits.group := i.U
    vcArbiter.io.in(i).valid := groupArray(i).io.vcEntry.valid
    vcArbiter.io.in(i).ready <> groupArray(i).io.vcEntry.ready
    groupArray(i).io.nNonZero.bits := nNonZeroPerGroup
    groupArray(i).io.nNonZero.valid := start
    groupArray(i).io.start := (state === sCompute)
    groupArray(i).io.ptrSpWrite.bits.addr :=  colPtrWriteAddr
    groupArray(i).io.ptrSpWrite.valid :=  colPtrWriteEn && (groupSel === i.U)
    groupArray(i).io.ptrSpWrite.bits.data := colPtrData
    groupArray(i).io.spWrite.bits.spSel :=     
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveVal -> 0.U,
        sDataMoveCol -> 2.U,
        sDataMoveRow -> 3.U,
        sDataMoveDen -> 1.U
      ))
    groupArray(i).io.spWrite.bits.spWriteCmd.addr :=
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveCol -> colPtrWriteAddr,
        sDataMoveRow -> rowWriteAddr,
        sDataMoveVal -> valWriteAddr,
        sDataMoveDen -> denWriteAddr
      ))
    groupArray(i).io.spWrite.bits.spWriteCmd.data := 
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveCol -> (colPtrDataBlock.reverse.reduce(Cat(_,_))),
        sDataMoveRow -> io.gbReadData.data,
        sDataMoveVal -> io.gbReadData.data,
        sDataMoveDen -> io.gbReadData.data
      ))
    groupArray(i).io.spWrite.valid := Mux(state === sDataMoveDen, true.B, 
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveCol -> colPtrWriteEn,
        sDataMoveRow -> true.B,
        sDataMoveVal -> true.B,
        sDataMoveDen -> true.B
      )).asBool  && (groupSel === i.U))
  }
val computeDone = groupArray.map(_.io.done).reduce(_&&_)
io.done := (state === sIdle) && !start

//state machine
  switch(state){
    is(sIdle){
      when(start){
        state := sDataMoveCol
        rowReadAddr := dec.io.sramCol  // TODO: Change this
        valReadAddr := dec.io.sramVal
        denReadAddr := dec.io.sramDen
        colPtrAddr := dec.io.sramPtr
      }
    }
    is(sDataMoveCol){
      when(colPtrFin){
        when(groupEnd){
          state := sDataMoveRow
          rowReadAddr := rowReadAddr + bankBlockSizeBytes.U
          nNonZeroPrevTotal := nNonZeroPrevTotal + dec.io.rowSize
        }
      }.otherwise{
        colPtrAddr := colPtrAddr + (cp.blockSize/8).U
      }
    }
    is(sDataMoveRow){
      when(rowFin){
        when(groupEnd){
          state := sDataMoveVal
          valReadAddr := valReadAddr + bankBlockSizeBytes.U
        }
        rowReadAddr := rowReadAddr + bankBlockSizeBytes.U
      }.otherwise{
        rowReadAddr := rowReadAddr + bankBlockSizeBytes.U
      }
    }
    is(sDataMoveVal){
      when(valFin){
        when(groupEnd){
          when(denseLoaded){
            state := sCompute
          }.otherwise{
            state := sDataMoveDen
          }
          denReadAddr := denReadAddr + bankBlockSizeBytes.U
        }
        valReadAddr := valReadAddr + bankBlockSizeBytes.U
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
    is(sCompute){
      when(!denseLoaded){
        computeTimeOut := computeTimeOut + 1.U
      }.otherwise{
        computeTimer := computeTimer + 1.U
      }
      when(computeDone){
        state := sCombineGroup
        denseLoaded := true.B
      }.elsewhen(denseLoaded){
        when(computeTimer === computeTimeOut){
          state := sIdle
          computeTimer := 0.U
          computeSkipCount := computeSkipCount + 1.U
        }
      }
    }
    is(sCombineGroup){
      when(nColInGroup === 0.U){
        state := sCombineGroup
      }.otherwise{
        state := sCombine
      }
    }
    is(sCombine){
      when(currColInGroup_q === (nColInGroup - 1.U)){
        when(aggDone){
          state := sIdle
        }.otherwise{
          state := sCombineGroup
        }
      }
    }
  }
  assert(computeSkipCount =/= 1000.U)
}
