package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._
import gcn.core.util._
import chisel3.util.experimental.group
// /** Compute.
//  *
//  * Takes instructions from fetch module. Schedules computation between PEs.
//  * Arbitrates communication betwen PE and scratchpads.
//  */
class PRTableEntry()(implicit p: Parameters) extends Bundle{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val nPartialRows = UInt(32.W)
  val isPRWithPrevGroup = Bool()
  val isPRWithNextGroup = Bool()
}
class PRTableEntryWithGroup()(implicit p: Parameters) extends Bundle{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val PRTableEntry = new PRTableEntry
  val group = UInt(cp.nGroups.W)
}

class Compute(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val cr = p(AccKey).crParams
  val regBits = p(AccKey).crParams.regBits
  val io = IO(new Bundle {
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    
    /*
    REMOVED:

    val gbReadCmd = Output(new SPReadCmd)
    val gbReadData = Input(new SPReadData(scratchType = "Global"))
    val pSumRead = ValidIO(UInt(26.W))
    val pSumReadData = Input(new SPReadData(scratchType = "Out"))
    val spOutWrite = Decoupled(new SPWriteCmd)
    */

    val valid = Input(Bool())
    val done = Output(Bool())
    val start = Input(Bool())
  })

  // need to REMOVE:
  io.done := true.B

  val bankBlockSizeBytes = cp.bankBlockSize/8

  // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  val dec = Module(new ComputeDecode)
  val denseLoaded = dec.io.denseLoaded

  dontTouch(dec.io.sramSum)
  dontTouch(dec.io.pSumInOutputSp)
  dontTouch(dec.io.partialSum)

  val groupArray = for(i <- 0 until cp.nGroups) yield {
    Module(new Group(groupID = i))
  }

// state machine
  val sIdle :: sDataMoveRow :: sDataMoveCol :: sDataMoveVal :: sDataMoveDen :: sDataMoveSum :: sCompute :: sCombineGroup :: sCombine :: sDone :: Nil = Enum(10)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val inst = RegEnable(inst_q.io.deq.bits, start)
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && io.valid


  val groupSel = RegInit(0.U(cp.nGroups.W))
  val groupEnd = groupSel === (cp.nGroups - 1).U
  
  val nNonZero = dec.io.colSize
  val nNonZeroPerGroup = dec.io.nNonZeroPerGroup
  
  /*
  REMOVED:
  
  val gbAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))
  val gbRdata = io.gbReadData.data

  // Row Splitting
  val rowPtrFin = Wire(Bool())
  val rowPtrWriteEn = !rowPtrFin && (state === sDataMoveRow)
  val nRowWritten_q = RegInit(0.U(32.W))
  val nRowRead_q = RegInit(0.U(32.W))
  val nRowWritten =  nRowWritten_q + !rowPtrFin
  val nRowWrittenValid = Wire(Bool())
  val rowOffset_q = RegInit(0.U(32.W))
  val rowPtrDataBlock = for(i <- 0 until (cp.bankBlockSize/cp.blockSize))yield{
    gbRdata((((i+1)*cp.blockSize) -1), i*cp.blockSize)
  }
  val rowPtrAddr = RegInit(0.U(32.W))
  val rowPtrIdxInBlock = rowPtrAddr(log2Ceil(bankBlockSizeBytes)-1,log2Ceil(cp.blockSize/8))
  val rowPtrData = MuxTree(rowPtrIdxInBlock, rowPtrDataBlock)
  val rowPtrReadAddr = Mux(start, dec.io.sramPtr, Mux(rowPtrFin, rowPtrAddr, rowPtrAddr + (cp.blockSize/8).U)) 
  //val groupSelPlusOneTimesNNzPerGroup = (groupSel + 1.U) << Log2(nNonZeroPerGroup)
  val groupSelPlusOneTimesNNzPerGroup = Reg(UInt(32.W))
  when(start){
    groupSelPlusOneTimesNNzPerGroup := nNonZeroPerGroup
  }

  rowPtrFin := (rowPtrData >= groupSelPlusOneTimesNNzPerGroup && groupSel < (cp.nGroups-1).U) || nRowRead_q >= dec.io.rowSize
  // Checks whether the final row in the group is a partial row
  val rowPtrIsPartialRow = rowPtrData > groupSelPlusOneTimesNNzPerGroup
  //val firstRowPtr_alreadyRead = RegEnable(rowPtrIsPartialRow || (groupSel === 0.U), rowPtrFin === 0.U)
  val firstRowPtr_alreadyRead = rowPtrIsPartialRow || (groupSelPlusOneTimesNNzPerGroup === rowPtrData) 
  val rowPtrWriteAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))
  when(state === sDataMoveRow){
    when(rowPtrFin){
      rowPtrWriteAddr := 0.U
    }.otherwise{
      rowPtrWriteAddr := rowPtrWriteAddr + (cp.blockSize/8).U
    }
  }

  // Col Splitting
  val colReadAddr = RegInit(0.U(32.W))
  val colWriteAddr = RegInit(0.U(32.W))
  val colReadBlockNum = RegInit(cp.nColInDense.U(32.W))
  val colFin = (colReadBlockNum >= (groupSelPlusOneTimesNNzPerGroup))
  when(state === sDataMoveCol){
    colReadBlockNum := colReadBlockNum + cp.nColInDense.U
  }.otherwise{
    colReadBlockNum := cp.nColInDense.U
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
  val valFin = (valReadBlockNum >= (groupSelPlusOneTimesNNzPerGroup))
  when(state === sDataMoveVal){
    valReadBlockNum := valReadBlockNum + cp.nColInDense.U
  }.otherwise{
    valReadBlockNum := cp.nColInDense.U
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

  // Partial Sum Splitting
  val pSumReadAddr = RegInit(0.U(26.W))
  io.pSumRead.bits := pSumReadAddr
  io.pSumRead.valid := dec.io.pSumInOutputSp && ((state === sDataMoveSum) || (state === sDataMoveDen) || (state === sDataMoveVal))
  val pSumWriteAddr = RegInit(0.U(26.W))
  val pSumRowsRead = RegInit(cp.nColInDense.U(32.W))
  val pSumFin = pSumRowsRead >= (dec.io.rowSize - 1.U)

  when(state === sDataMoveSum){
     pSumRowsRead :=  pSumRowsRead + 1.U
      when(pSumFin){
        pSumWriteAddr := 0.U
      }.otherwise{
        pSumWriteAddr := pSumWriteAddr + bankBlockSizeBytes.U
      }
     pSumReadAddr := pSumReadAddr + bankBlockSizeBytes.U
  }.otherwise{
     pSumRowsRead := 1.U
     pSumWriteAddr := 0.U
     pSumReadAddr := Mux(dec.io.pSumInOutputSp, 0.U, dec.io.sramSum)
  }
dontTouch(pSumFin)

// group select
  when((((state === sDataMoveRow) && rowPtrFin)||(state === sDataMoveCol) && colFin)||((state === sDataMoveVal) && valFin) || ((state === sDataMoveSum) && pSumFin)){
    // row offset = row offset + nRowWritten + 1 (since it starts at 0) - the bounds that don't count
    // rowOffset_q := rowOffset_q  + (nRowWritten_q + 1.U -& firstRowPtr_alreadyRead -& rowPtrIsPartialRow)
    rowOffset_q := rowOffset_q  + (nRowWritten_q + 1.U -& firstRowPtr_alreadyRead -& rowPtrIsPartialRow)
    when(groupEnd){
      groupSel := 0.U
      nRowWritten_q := 0.U
      groupSelPlusOneTimesNNzPerGroup := nNonZeroPerGroup
    }.otherwise{
      groupSel := groupSel + 1.U
      groupSelPlusOneTimesNNzPerGroup := groupSelPlusOneTimesNNzPerGroup + nNonZeroPerGroup
      nRowWritten_q := 0.U
    }
  }.elsewhen((state === sDataMoveRow)){
    nRowWritten_q := nRowWritten
  }
  nRowWrittenValid := ((state === sDataMoveRow) && rowPtrFin)
  
  io.gbReadCmd.addr := MuxLookup(true.B,
      rowPtrReadAddr, // default
      Array(
        ((state === sIdle) && start)-> rowPtrReadAddr,
        ((state === sDataMoveRow) && (!(rowPtrFin && groupEnd))) -> rowPtrReadAddr,
        ((state === sDataMoveRow) && (rowPtrFin && groupEnd)) -> colReadAddr,
        ((state === sDataMoveCol) && !(colFin && groupEnd)) -> colReadAddr,
        ((state === sDataMoveCol) && (colFin && groupEnd)) -> valReadAddr,
        ((state === sDataMoveVal) && !(valFin && groupEnd)) -> valReadAddr,
        ((state === sDataMoveVal) && (valFin && groupEnd && !denseLoaded)) -> denReadAddr,
        ((state === sDataMoveVal) && (valFin && groupEnd && denseLoaded)) -> pSumReadAddr,
        ((state === sDataMoveDen) && !(denFin)) -> denReadAddr,
        ((state === sDataMoveDen) && denFin) -> pSumReadAddr,
        ((state === sDataMoveSum)) -> pSumReadAddr
      ))


// Partial output aggregation

val aggGroup = RegInit(0.U(log2Ceil(cp.nGroups).W))
val sAggPrev :: sAggNext :: Nil = Enum(2)
val aggState = RegInit(sAggNext)

val prSeq = groupArray.map(_.io.prEntry)

val aggPRTable = MuxTree(aggGroup, groupArray.map(_.io.prEntry))
val prWithPrev = MuxTree(aggGroup, groupArray.map(_.io.partialRowWithPrev))
val prWithNext = MuxTree(aggGroup, groupArray.map(_.io.partialRowWithNext))

dontTouch(prWithPrev)
dontTouch(prWithNext)

//for (i <- 0 until cp.nGroups){groupArray(i).io.prEntry.ready := false.B}

val aggBuffer = Reg(chiselTypeOf(groupArray(0).io.partialRowWithPrev.data))
val aggAddressBuffer = Reg(chiselTypeOf(groupArray(0).io.partialRowWithPrev.address))

val aggBufferSeq = for(i <- 0 until cp.nPE)yield{aggBuffer(((i+1)*cp.blockSize)-1, i*cp.blockSize)}
val prWithPrevSeq = for(i <- 0 until cp.nPE)yield{prWithPrev.data(((i+1)*cp.blockSize)-1, i*cp.blockSize)}

val aggBufferPlusPRWithPrev = aggBufferSeq.zip(prWithPrevSeq).map{case(d,dP) => d+dP}.reverse.reduce{Cat(_,_)}
val aggDone = Wire(Bool())

val aggQueue = Module(new Queue(new SPWriteCmd, cp.aggregationBufferDepth))

assert(aggQueue.io.enq.ready === true.B, "ERROR: Aggregation queue full!")


aggQueue.io.enq.bits.data := aggBufferPlusPRWithPrev
aggQueue.io.enq.bits.addr := prWithPrev.address


// Two stage output arbiters - aggregation gets priority
val groupArbiter = Module(new MyRRArbiter(new SPWriteCmd, cp.nGroups))
val outputArbiter = Module(new Arbiter(new SPWriteCmd, 2))

outputArbiter.io.in(0) <> aggQueue.io.deq

// Extra register stage between group arbiter and output arbiter
val groupOutputReg = RegEnable(groupArbiter.io.out.bits, outputArbiter.io.in(1).ready)
val groupOutputReg_valid = RegEnable(groupArbiter.io.out.valid, outputArbiter.io.in(1).ready)
outputArbiter.io.in(1).bits := groupOutputReg
outputArbiter.io.in(1).valid := groupOutputReg_valid
groupArbiter.io.out.ready := outputArbiter.io.in(1).ready
val delayedOutputArbiterData = RegNext(outputArbiter.io.out.bits.data)


// Partial sum buffer
val spPSum = Module(new BankedScratchpad(scratchType = "Den"))
spPSum.io.spWrite.data := Mux(dec.io.pSumInOutputSp, Mux(RegNext(pSumReadAddr(5)),io.pSumReadData.data(511,256),io.pSumReadData.data(255,0)), io.gbReadData.data)
spPSum.io.spWrite.addr := pSumWriteAddr
spPSum.io.writeEn := state === sDataMoveSum

// Read partial sum row
val denCol  = RegInit(VecInit(Seq.tabulate(cp.nPE)(n => n.U(M_SRAM_OFFSET_BITS.W)))) 
for (i <- 0 until cp.nPE){
    spPSum.io.spReadCmd(i).addr := outputArbiter.io.out.bits.addr + (denCol(i) << log2Ceil(cp.blockSize/8))
}
val pSumRow = spPSum.io.spReadData.map(_.data)
val delayedOutputArbiterRow = for(i <- 0 until cp.nColInDense)yield{delayedOutputArbiterData(((i+1)*cp.blockSize)-1, i*cp.blockSize)}
val accumulatedRow = delayedOutputArbiterRow.zip(pSumRow).map{case(x,y) => Mux(dec.io.partialSum, x+y, x)}
val formattedOutput = accumulatedRow.map(_(cp.blockSize-1, 0)).reverse.reduce(Cat(_,_))

//Output connections
io.spOutWrite.valid := RegNext(outputArbiter.io.out.valid)
io.spOutWrite.bits.addr := RegNext(outputArbiter.io.out.bits.addr)
io.spOutWrite.bits.data := formattedOutput
outputArbiter.io.out.ready := io.spOutWrite.ready


when(state === sCombine){
  // Defaults
  aggDone := false.B
  aggQueue.io.enq.valid := false.B

  when(aggState === sAggNext){

    // Store data and increment group
    aggBuffer := prWithNext.data
    aggGroup := aggGroup + 1.U
    when(aggGroup === (cp.nGroups-1).U){
      aggDone := true.B
    }

    // Skip sAggPrev stage if there isn't a partial row between the groups
    when(aggPRTable.isPRWithNextGroup){
      aggState := sAggPrev
    }.otherwise{
      aggState := sAggNext
    }
  }.elsewhen(aggState === sAggPrev){

    // When this group has a single partial row that extends from the previous group to the next group
    when (aggPRTable.isPRWithNextGroup && (aggPRTable.nPartialRows === 1.U)){
      aggState := sAggPrev
      aggBuffer := aggBufferPlusPRWithPrev

      aggGroup := aggGroup + 1.U
      when(aggGroup === (cp.nGroups-1).U){
        aggDone := true.B
      }
    

    // Send the output to the queue it is ready, stall if it is not ready
    }.elsewhen(aggQueue.io.enq.ready){

      aggQueue.io.enq.valid := true.B

      aggState := sAggNext

      when (!aggPRTable.isPRWithNextGroup){
        aggGroup := aggGroup + 1.U
        when(aggGroup === (cp.nGroups-1).U){
          aggDone := true.B
        }
      }
    }
  }
}.otherwise{
  aggGroup := 0.U
  aggState := sAggNext
  aggDone := false.B
  aggQueue.io.enq.valid := false.B
}

  val allOutputQueuesEmpty = groupArray.map(_.io.outputQueueEmpty).reduce(_&&_)
  dontTouch(allOutputQueuesEmpty)

// group io
  for(i <- 0 until cp.nGroups){

    groupArray(i).io.rowPtrBegin.bits := groupSelPlusOneTimesNNzPerGroup
    if (i > 0)
      groupArray(i).io.rowPtrBegin.valid := (groupSel === (i-1).U)
    else
      groupArray(i).io.rowPtrBegin.valid := true.B
    groupArray(i).io.outputsComplete := allOutputQueuesEmpty
    groupArray(i).io.rowPtrEnd.bits := groupSelPlusOneTimesNNzPerGroup
    groupArray(i).io.rowPtrEnd.valid := (groupSel === i.U)
    groupArbiter.io.in(i) <> groupArray(i).io.outputBufferWriteData
    groupArray(i).io.nRowPtrInGroup.bits := nRowWritten
    groupArray(i).io.nRowPtrInGroup.valid := (nRowWrittenValid && groupSel === i.U)
    groupArray(i).io.nNonZero.bits := nNonZeroPerGroup
    groupArray(i).io.nNonZero.valid := start
    groupArray(i).io.start := (state === sCompute)
    groupArray(i).io.ptrSpWrite.bits.addr :=  rowPtrWriteAddr
    groupArray(i).io.ptrSpWrite.valid :=  rowPtrWriteEn && (groupSel === i.U)
    groupArray(i).io.ptrSpWrite.bits.data := rowPtrData
    groupArray(i).io.isPRWithNextGroup.valid := (state === sDataMoveRow) && (groupSel === i.U)
    groupArray(i).io.isPRWithNextGroup.bits := rowPtrIsPartialRow 
    //groupArray(i).io.rowOffset.bits := rowOffset_q
    groupArray(i).io.rowOffset.bits := nRowRead_q - RegNext(rowPtrIsPartialRow)
    groupArray(i).io.rowOffset.valid := (state === sDataMoveRow) && (groupSel === i.U) && RegNext(rowPtrFin)
    groupArray(i).io.spWrite.bits.spSel :=     
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveVal -> 0.U,
        sDataMoveRow -> 2.U,
        sDataMoveCol -> 3.U,
        sDataMoveDen -> 1.U,
        sDataMoveSum -> 4.U
      ))
    groupArray(i).io.spWrite.bits.spWriteCmd.addr :=
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveRow -> rowPtrWriteAddr,
        sDataMoveCol -> colWriteAddr,
        sDataMoveVal -> valWriteAddr,
        sDataMoveDen -> denWriteAddr
      ))
    groupArray(i).io.spWrite.bits.spWriteCmd.data := 
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveRow -> (rowPtrDataBlock.reverse.reduce(Cat(_,_))),
        sDataMoveCol -> io.gbReadData.data,
        sDataMoveVal -> io.gbReadData.data,
        sDataMoveDen -> io.gbReadData.data
      ))
    groupArray(i).io.spWrite.valid := Mux(state === sDataMoveDen, true.B, 
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveRow -> rowPtrWriteEn,
        sDataMoveCol -> true.B,
        sDataMoveVal -> true.B,
        sDataMoveDen -> true.B,
        sDataMoveSum -> false.B
      )).asBool  && (groupSel === i.U))
  }
val computeDone = groupArray.map(_.io.done).reduce(_&&_)
io.done := (state === sIdle) && !io.start

//state machine
  switch(state){
    is(sIdle){
      rowOffset_q := 0.U
      when(start){
        state := sDataMoveRow
        colReadAddr := dec.io.sramCol
        valReadAddr := dec.io.sramVal
        denReadAddr := dec.io.sramDen
        rowPtrAddr := dec.io.sramPtr
        nRowRead_q := 0.U
      }
    }
    is(sDataMoveRow){
      when(rowPtrFin){
        when(groupEnd){
          state := sDataMoveCol
          colReadAddr := colReadAddr + bankBlockSizeBytes.U
        }
      }.otherwise{
        rowPtrAddr := rowPtrAddr + (cp.blockSize/8).U
        nRowRead_q := nRowRead_q + 1.U
        dontTouch(nRowRead_q)
      }
    }
    is(sDataMoveCol){
      when(colFin){
        when(groupEnd){
          state := sDataMoveVal
          valReadAddr := valReadAddr + bankBlockSizeBytes.U
        }
        colReadAddr := colReadAddr + bankBlockSizeBytes.U
      }.otherwise{
        colReadAddr := colReadAddr + bankBlockSizeBytes.U
      }
    }
    is(sDataMoveVal){
      when(valFin){
        when(groupEnd){
          when(denseLoaded){
            state := sDataMoveSum
            pSumReadAddr := pSumReadAddr + bankBlockSizeBytes.U
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
        pSumReadAddr := pSumReadAddr + bankBlockSizeBytes.U
        
        when(dec.io.partialSum){
          state := sDataMoveSum
        }.otherwise{
          state := sCompute
        }
      }
    }
    is(sDataMoveSum){
      when(pSumFin){
        state := sCompute
      }
    }
    is(sCompute){
      when(computeDone){
        state := sCombine
      }
    }
    is(sCombine){
      when (aggDone){
        state := sIdle
      }
    }
  }
  */
}
