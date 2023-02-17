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
  val groupArray = for(i <- 0 until cp.nGroups) yield {
    Module(new Group(groupID = i))
  }

// state machine
  val sIdle :: sDataMoveRow :: sDataMoveCol :: sDataMoveVal :: sDataMoveDen :: sCompute :: sCombineGroup :: sCombine :: sDone :: Nil = Enum(9)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val inst = RegEnable(inst_q.io.deq.bits, start)
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && io.valid


  val groupSel = RegInit(0.U(cp.nGroups.W))
  val groupEnd = groupSel === (cp.nGroups - 1).U
  val nNonZeroPrevTotal = RegInit(0.U(32.W))
  
  val nNonZeroPerGroup =  dec.io.colSize >> log2Ceil(cp.nGroups)
  val gbAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))
  val gbRdata = io.gbReadData.data

  // Row Splitting
  val rowPtrFin = Wire(Bool())
  val rowPtrDataBlock = for(i <- 0 until (cp.bankBlockSize/cp.blockSize))yield{
    gbRdata((((i+1)*cp.blockSize) -1), i*cp.blockSize)
  }
  val rowPtrAddr = RegInit(0.U(32.W))
  val rowPtrIdxInBlock = rowPtrAddr(log2Ceil(bankBlockSizeBytes)-1,log2Ceil(cp.blockSize/8))
  val rowPtrData = MuxTree(rowPtrIdxInBlock, rowPtrDataBlock)
  val rowPtrReadAddr = Mux(start, dec.io.sramPtr, Mux(rowPtrFin, rowPtrAddr, rowPtrAddr + (cp.blockSize/8).U)) 
  val rowPtrFinComparison = (nNonZeroPrevTotal + (( groupSel + 1.U) << Log2(nNonZeroPerGroup)))
  rowPtrFin := rowPtrData >= rowPtrFinComparison
  // Checks whether the final row in the group is a partial row
  val rowPtrIsPartialRow = rowPtrData > rowPtrFinComparison
  val rowPtrWriteAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))
  when(state === sDataMoveRow){
    when(rowPtrFin){
      rowPtrWriteAddr := 0.U
    }.otherwise{
      rowPtrWriteAddr := rowPtrWriteAddr + (cp.blockSize/8).U
    }
  }
  val rowPtrWriteEn = !rowPtrFin && (state === sDataMoveRow)
  val nRowWritten_q = RegInit(0.U(32.W))
  val nRowWritten =  nRowWritten_q + !rowPtrFin
  val nRowWrittenValid = Wire(Bool())
  val rowOffset_q = RegInit(0.U(32.W))

  // Col Splitting
  val colReadAddr = RegInit(0.U(32.W))
  val colWriteAddr = RegInit(0.U(32.W))
  val colReadBlockNum = RegInit(cp.nColInDense.U(32.W))
  val colFin = (colReadBlockNum >= ((groupSel + 1.U) << Log2(nNonZeroPerGroup)))
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
  val valFin = (valReadBlockNum >= ((groupSel + 1.U) << Log2(nNonZeroPerGroup)))
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

// group select
  when((((state === sDataMoveRow) && rowPtrFin)||(state === sDataMoveCol) && colFin)||((state === sDataMoveVal) && valFin)){
    rowOffset_q := rowOffset_q + (nRowWritten_q - rowPtrIsPartialRow)
    when(groupEnd){
      groupSel := 0.U
      nRowWritten_q := 0.U
    }.otherwise{
      groupSel := groupSel + 1.U
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
        ((state === sDataMoveVal) && (valFin && groupEnd)) -> denReadAddr,
        ((state === sDataMoveDen)) -> denReadAddr
      ))

// Output arbiter
val outputArbiter = Module(new MyRRArbiter(new SPWriteCmd, cp.nGroups + 1))

//Output connections
io.spOutWrite <> outputArbiter.io.out


// Partial output aggregation

val aggGroup = RegInit(0.U(log2Ceil(cp.nGroups).W))
val sAggPrev :: sAggNext :: Nil = Enum(2)
val aggState = RegInit(sAggNext)

val prSeq = groupArray.map(_.io.prEntry)

val aggPRTable = MuxTree(aggGroup, groupArray.map(_.io.prEntry.bits))
val prWithPrev = MuxTree(aggGroup, groupArray.map(_.io.partialRowWithPrev))
val prWithNext = MuxTree(aggGroup, groupArray.map(_.io.partialRowWithNext))

dontTouch(prWithPrev)
dontTouch(prWithNext)

for (i <- 0 until cp.nGroups){groupArray(i).io.prEntry.ready := false.B}

val aggBuffer = Reg(chiselTypeOf(groupArray(0).io.partialRowWithPrev.data))
val aggAddressBuffer = Reg(chiselTypeOf(groupArray(0).io.partialRowWithPrev.address))

val aggBufferSeq = for(i <- 0 until cp.nColInDense)yield{aggBuffer(((i+1)*cp.blockSize)-1, i*cp.blockSize)}
val prWithPrevSeq = for(i <- 0 until cp.nColInDense)yield{prWithPrev.data(((i+1)*cp.blockSize)-1, i*cp.blockSize)}

val aggBufferPlusPRWithPrev = aggBufferSeq.zip(prWithPrevSeq).map{case(d,dP) => d+dP}.reverse.reduce{Cat(_,_)}
val aggDone = Wire(Bool())

val aggQueue = Module(new Queue(new SPWriteCmd, cp.aggregationBufferDepth))


aggQueue.io.enq.bits.data := aggBufferPlusPRWithPrev
aggQueue.io.enq.bits.addr := prWithPrev.address

// Arbiter connection to partial row aggregation output
outputArbiter.io.in(cp.nGroups) <> aggQueue.io.deq



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

// group io
  for(i <- 0 until cp.nGroups){

    outputArbiter.io.in(i) <> groupArray(i).io.outputBufferWriteData
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
    groupArray(i).io.rowOffset.bits := rowOffset_q
    groupArray(i).io.rowOffset.valid := (state === sDataMoveRow) && (groupSel === i.U)
    groupArray(i).io.spWrite.bits.spSel :=     
      MuxLookup(state,
      0.U, // default
      Array(
        sDataMoveVal -> 0.U,
        sDataMoveRow -> 2.U,
        sDataMoveCol -> 3.U,
        sDataMoveDen -> 1.U
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
        sDataMoveDen -> true.B
      )).asBool  && (groupSel === i.U))
  }
val computeDone = groupArray.map(_.io.done).reduce(_&&_)
io.done := (state === sIdle) && !start

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
      }
    }
    is(sDataMoveRow){
      when(rowPtrFin){
        when(groupEnd){
          state := sDataMoveCol
          colReadAddr := colReadAddr + bankBlockSizeBytes.U
          nNonZeroPrevTotal := nNonZeroPrevTotal + dec.io.colSize
        }
      }.otherwise{
        rowPtrAddr := rowPtrAddr + (cp.blockSize/8).U
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
        state := sCombine//Group
        denseLoaded := true.B
      }.elsewhen(denseLoaded){
        when(computeTimer === computeTimeOut){
          state := sIdle
          computeTimer := 0.U
          computeSkipCount := computeSkipCount + 1.U
        }
      }
    }
    is(sCombine){
      when (aggDone){
        state := sIdle
      }
    }
  }
  assert(computeSkipCount =/= 1000.U)
}
