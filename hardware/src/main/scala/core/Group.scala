package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._
import ISA._
import gcn.core.util.MuxTree
import os.write

class rowPtrData(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val rowPtr1Data = UInt(32.W)
  val rowPtr2Data = UInt(32.W)
  val isPR = Bool()
}

class partialRowOutput(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  //val data = Vec(cp.nColInDense, UInt(cp.blockSize.W))
  val data = UInt((cp.nColInDense * cp.blockSize).W)
}

class partialRowOutputWithAddress(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  //val data = Vec(cp.nColInDense, UInt(cp.blockSize.W))
  val data = UInt((cp.nColInDense * cp.blockSize).W)
  val address =  UInt(M_SRAM_OFFSET_BITS.W)
}


// /* Group
//  */
class Group(val groupID: Int = 0)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val nBanks = cp.nColInDense
  val io = IO(new Bundle {
    val nRowPtrInGroup = Flipped(ValidIO(UInt(32.W)))
    val rowOffset = Flipped(ValidIO(UInt(32.W)))
    val selectedRowOffset = Output(UInt(32.W))
    val spWrite = Flipped(Decoupled(new SPWriteCmdWithSel))
    val ptrSpWrite = Flipped(Decoupled(new SPWriteCmd(mode = "single")))
    val nNonZero = Flipped(ValidIO(UInt(32.W)))
    val rowPtrBegin = Flipped(ValidIO(UInt(32.W)))
    val rowPtrEnd = Flipped(ValidIO(UInt(32.W)))
    val isPRWithNextGroup = Flipped(ValidIO(Bool()))
    val denWrite = Input(new SPWriteCmd(scratchType = "Global"))
    val denWriteEn = Input(Bool())
    val denseGroup = Input(UInt(ISA.DEN_GROUP_SEL_BITS.W))
    val prEntry = Output(new PRTableEntry)
    val start = Input(Bool())
    val done = Output(Bool())
    val outputBufferWriteData = Vec(cp.groupSize, Decoupled(new SPWriteCmd))
    val partialRowWithPrev = Vec(cp.groupSize, Output(new partialRowOutputWithAddress))
    val partialRowWithNext = Vec(cp.groupSize, Output(new partialRowOutput))
    val outputQueueEmpty = Output(Bool())
    val outputsComplete = Input(Bool())
  })
  
  val startSignal = io.start && !RegNext(io.start)
  val startLatch = RegInit(false.B)
  when(startSignal && !io.outputsComplete){
    startLatch := true.B
  }.elsewhen(io.outputsComplete){
    startLatch := false.B
  }
  val pulse = (startSignal || startLatch) && io.outputsComplete

  val rowPtrSize = RegEnable(io.nRowPtrInGroup.bits, io.nRowPtrInGroup.valid)
  val nNonZero = RegEnable(io.nNonZero.bits, io.nNonZero.valid)
  val rowPtrBegin = if (groupID != 0)
                      RegEnable(io.rowPtrBegin.bits, io.rowPtrBegin.valid)
                    else
                      0.U(32.W)
  val rowPtrEnd = RegEnable(io.rowPtrEnd.bits, io.rowPtrEnd.valid)
  val totalNonZero = RegInit(0.U(cp.blockSize.W))
  val isPRWithNextGroup = RegEnable(io.isPRWithNextGroup.bits, io.isPRWithNextGroup.valid)
  dontTouch(isPRWithNextGroup)
  val rowOffset = if (groupID > 0) RegEnable(io.rowOffset.bits, io.rowOffset.valid) else 0.U
  io.selectedRowOffset := rowOffset

  /*when(io.nNonZero.valid){
    rowPtrBegin := groupID.U << Log2(io.nNonZero.bits)
    rowPtrEnd := (groupID + 1).U << Log2(io.nNonZero.bits)
  }*/
  /*when(io.nNonZero.valid){
    rowPtrBegin := rowPtrBegin + (groupID.U << Log2(io.nNonZero.bits))
    rowPtrEnd := rowPtrEnd + ((groupID + 1).U << Log2(io.nNonZero.bits))
  }.elsewhen(io.done && !RegNext(io.done)){ // Rising edge
    totalNonZero := totalNonZero + (io.nNonZero.bits << log2Ceil(cp.nGroups))
    rowPtrBegin := totalNonZero + (io.nNonZero.bits << log2Ceil(cp.nGroups))
    rowPtrEnd := totalNonZero + (io.nNonZero.bits << log2Ceil(cp.nGroups))
  }*/
  val d1_rowPtrAddr = Wire(UInt(32.W))
  //val prQueue = Module(new Queue(new PRTableEntry, 4)) 
 // prQueue.io.deq <> io.prEntry
  
  // ScratchPads
  val spVal = Module(new Scratchpad(scratchType = "Val", masked = false))
  val spCol = Module(new Scratchpad(scratchType = "Col", masked = false))
  val spPtr = Module(new SingleScratchpad(scratchType = "Ptr", masked = false))
  val spDen = for(i <- 0 until cp.groupSize) yield {
    Module(new DenseScratchpad(scratchType = "Den"))
  }


  io.spWrite.bits.spWriteCmd <> spVal.io.spWrite
  io.spWrite.bits.spWriteCmd <> spCol.io.spWrite
  spDen.map{_.io.spWrite <> io.denWrite}
  
  io.ptrSpWrite.bits <> spPtr.io.spWrite
  io.spWrite.ready := true.B
  io.ptrSpWrite.ready := true.B
  spVal.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 0.U)

  // TODO: FIGURE THIS OUT
  for(i <- 0 until cp.groupSize){
    spDen(i).io.writeEn := io.denWriteEn && (io.denseGroup === i.U)
  }
  spPtr.io.writeEn := io.ptrSpWrite.fire
  spCol.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 3.U)

  spVal.io.spReadCmd.addr := io.spWrite.bits.spWriteCmd.addr
  spPtr.io.spReadCmd.addr := d1_rowPtrAddr
  spCol.io.spReadCmd.addr := io.spWrite.bits.spWriteCmd.addr

  val blockSizeBytes = (cp.blockSize/8)

  /* Pipeline Stage: D1
  Cycles = 2
  Inputs:
    1. Row assignment from Compute module via peReq
  Performs:
    1. Uses a statemachine to read two consecutive addresses in row_ptr starting from assigned row
    2. Sends rowPtrData1, rowPtrData2 to next pipeline stage D2
  Output:
    1. rowPtrData1, rowPtrData2, (currRowPtr = rowPtr1Data) and peReq goes to D2.
  */
  val d1Queue = Module(new Queue(new rowPtrData(), 2048))
  assert(d1Queue.io.enq.ready === true.B)
  val d1_reqValid_q = Reg(Bool())
  val sIdle :: sRowPtr1 :: sRowPtr2  :: Nil = Enum(3)
  val d1_state_q = RegInit(sIdle)
  val d1_statePrev_q = RegNext(d1_state_q)
  val d1_rowPtrAddr_q = RegInit(0.U(M_SRAM_OFFSET_BITS.W))
  val d1_rowPtrInc = Wire(Bool())
  d1_rowPtrAddr := Mux(d1_rowPtrInc, d1_rowPtrAddr_q + blockSizeBytes.U, d1_rowPtrAddr_q) 
  val d1_rowPtr1Data_q = Reg(chiselTypeOf(spPtr.io.spReadData.data))
  val d1_rowPtr2Data_q = Reg(chiselTypeOf(spPtr.io.spReadData.data))
  dontTouch(d1_rowPtr1Data_q)
  val isPRWithPrevGroup = (spPtr.io.spReadData.data =/= rowPtrBegin)
  val isPRWithPrevGroup_q = RegEnable(isPRWithPrevGroup, pulse)
  //val isPR_withNextRow_q = Reg(Bool())
  val d1_numRowPtr_q = RegInit(0.U(32.W))
  val d1_isPR = Reg(Bool())
  val rowPtrDone = d1_numRowPtr_q >= rowPtrSize
  d1Queue.io.enq.bits.rowPtr1Data := d1_rowPtr1Data_q
  d1Queue.io.enq.bits.rowPtr2Data := d1_rowPtr2Data_q
  d1Queue.io.enq.bits.isPR := d1_isPR
  d1Queue.io.enq.valid := d1_reqValid_q

  when((d1_state_q === sRowPtr1 || d1_state_q === sRowPtr2) && !rowPtrDone){
    d1_numRowPtr_q := d1_numRowPtr_q + 1.U
  }.elsewhen(d1_state_q === sIdle){
    d1_numRowPtr_q := 0.U
  }

  // Increment row pointer to next block
  when((d1_state_q === sRowPtr1) || (d1_state_q === sRowPtr2)){
    d1_rowPtrAddr_q := d1_rowPtrAddr
    d1_rowPtrInc := true.B
  }.otherwise{
    d1_rowPtrInc := false.B
  }

  switch(d1_state_q){
    is(sIdle){
      d1_isPR := false.B
      d1_rowPtrAddr_q := 0.U
      when(pulse){
        //isPR_withNextRow_q := false.B;
        when(rowPtrSize === 0.U){
          // Partial row pointer on both ends
          d1_rowPtr1Data_q := rowPtrBegin - rowPtrBegin
          d1_rowPtr2Data_q := rowPtrEnd - rowPtrBegin
          d1_reqValid_q := true.B
          d1_isPR := true.B
          //isPR_withNextRow_q := true.B;
        }.otherwise{
          when(isPRWithPrevGroup){
            // Partial row pointer at start, continue normally
            d1_rowPtr1Data_q := rowPtrBegin - rowPtrBegin
            d1_rowPtr2Data_q := spPtr.io.spReadData.data - rowPtrBegin
            d1_reqValid_q := true.B
            d1_state_q := sRowPtr1
            d1_isPR := true.B
          }.otherwise{
            d1_state_q := sRowPtr1
            d1_isPR := false.B
          }
        }
      }.otherwise{
        d1_reqValid_q := false.B
      }
    }
    is(sRowPtr1){
      when(rowPtrSize === 1.U){
        // Needs partial row pointer on the end of the row
        d1_rowPtr1Data_q := spPtr.io.spReadData.data - rowPtrBegin
        d1_rowPtr2Data_q := rowPtrEnd - rowPtrBegin
        d1_state_q := sIdle
        d1_reqValid_q := true.B
        d1_isPR := isPRWithNextGroup
        //isPR_withNextRow_q := true.B
      }.otherwise{
        d1_reqValid_q := false.B
        d1_state_q := sRowPtr2
        d1_rowPtr1Data_q := spPtr.io.spReadData.data - rowPtrBegin
        d1_isPR := false.B
      }
    }
    is(sRowPtr2){
      when(rowPtrDone){
        d1_isPR := isPRWithNextGroup
        when(d1_rowPtr2Data_q =/= (rowPtrEnd - rowPtrBegin)){
          // Needs partial row pointer on the end of the row
          d1_rowPtr1Data_q := d1_rowPtr2Data_q
          d1_rowPtr2Data_q := rowPtrEnd - rowPtrBegin
          d1_state_q := sIdle
          d1_reqValid_q := true.B
          //isPR_withNextRow_q := true.B
        }.otherwise{
          d1_state_q := sIdle
          d1_reqValid_q := false.B
          //isPR_withNextRow_q := false.B
        }
      }.otherwise{
        d1_isPR := false.B
        when(d1_statePrev_q === d1_state_q){
          d1_rowPtr1Data_q := d1_rowPtr2Data_q
        }
        d1_rowPtr2Data_q := spPtr.io.spReadData.data - rowPtrBegin
        d1_reqValid_q := true.B
        d1_state_q := sRowPtr2
      }
    }
  }

  val d1_numRow_q = RegInit(0.U(32.W))
  when(d1Queue.io.enq.fire){
    d1_numRow_q := d1_numRow_q + 1.U
  }.elsewhen(pulse){
    d1_numRow_q := 0.U
  }
  val d1_numRow = Mux(d1Queue.io.enq.fire, d1_numRow_q + 1.U, d1_numRow_q)

  spPtr.io.spReadCmd.addr  := Mux(d1_state_q === sIdle, d1_rowPtrAddr_q, d1_rowPtrAddr)
  val d1_currRowPtr = d1Queue.io.deq.bits.rowPtr1Data
  val d1_currDenCol = 0.U(cp.blockSize.W)
  val decompressDone = ((d1_state_q === sIdle) && ((d1_statePrev_q === sRowPtr2) || ((pulse) && rowPtrSize === 0.U) || (d1_statePrev_q === sRowPtr1)))
  
  
  /*
  prQueue.io.enq.valid := RegNext(decompressDone)
  prQueue.io.enq.bits.isPRWithPrevGroup := isPRWithPrevGroup_q
  prQueue.io.enq.bits.isPRWithNextGroup := isPRWithNextGroup
  //prQueue.io.enq.bits.nPartialRows := d1_numRow
  prQueue.io.enq.bits.nPartialRows := Mux(rowPtrSize === 0.U, (isPRWithNextGroup || isPRWithPrevGroup_q) , isPRWithNextGroup +& isPRWithPrevGroup_q)
*/
  val prEntryValid = RegNext(decompressDone)
  io.prEntry.isPRWithPrevGroup := RegEnable(isPRWithPrevGroup_q, prEntryValid)
  io.prEntry.isPRWithNextGroup := RegEnable(isPRWithNextGroup, prEntryValid)
  io.prEntry.nPartialRows := RegEnable(Mux(rowPtrSize === 0.U, (isPRWithNextGroup || isPRWithPrevGroup_q) , isPRWithNextGroup +& isPRWithPrevGroup_q), prEntryValid)

  /* Pipeline Stage: D2
  Cycles = 1
  Inputs:
    1. rowPtr1Data, rowPtr2Data and (currRowPtr = rowPtr1Data), peReq from D1 stage
    2. rowPtr1Data, rowPtr2Data, updated currRowPtr, peReq from D2 stage
  Performs:
    1. Arbitrates between requests from D1 stage and prev D2 stage. prev D2 stage is always given priority
    2. Reads the colIdx and sends the data to stage DR
  Output:
    1. colIdx and peReq goes to DR.
  */
  val d2_rowNum_q = RegInit(0.U(M_SRAM_OFFSET_BITS.W))
  when(d1Queue.io.deq.fire){
    d2_rowNum_q := d2_rowNum_q + 1.U
  }.elsewhen(pulse){
    d2_rowNum_q := 0.U
  }
  val d2_nextValid = Wire(Bool())
  val d2_nextValid_q = RegInit(false.B)
  val d2_nextRowPtr_q = Reg(chiselTypeOf(d1_currRowPtr))
  val d2_valid = WireDefault(false.B)
  val d2_rowPtr1Data_q = RegInit(0.U(cp.blockSize.W))
  val d2_rowPtr2Data_q = RegInit(0.U(cp.blockSize.W))
  val d2_isPR_q = RegInit(false.B)
  val d2_currRowPtr_q = RegInit(0.U(cp.blockSize.W))
  val d2_currRowPtr = Mux(d2_nextValid_q, d2_nextRowPtr_q, d1_currRowPtr)
  val d2_rowPtr1Data = Mux(d2_nextValid_q, d2_rowPtr1Data_q, d1Queue.io.deq.bits.rowPtr1Data)
  val d2_rowPtr2Data = Mux(d2_nextValid_q, d2_rowPtr2Data_q, d1Queue.io.deq.bits.rowPtr2Data)
  val d2_isPR = Mux(d2_nextValid_q, d2_isPR_q, d1Queue.io.deq.bits.isPR)
  val d2_endOfRow_q = RegInit(false.B)
  val d2_isNewOutput_q = RegNext(d2_currRowPtr === d2_rowPtr1Data)
  d1Queue.io.deq.ready := !d2_nextValid_q
  val d1_valid = d1Queue.io.deq.valid
  d2_valid := d2_nextValid_q || (d1Queue.io.deq.valid && !d2_nextValid_q)
  d2_rowPtr1Data_q := d2_rowPtr1Data
  d2_rowPtr2Data_q := d2_rowPtr2Data
  d2_isPR_q := d2_isPR
  d2_currRowPtr_q := d2_currRowPtr
  spCol.io.spReadCmd.addr := (d2_currRowPtr << log2Ceil(blockSizeBytes))
  val d2_endOfRow = (d2_currRowPtr === (d2_rowPtr2Data - 1.U))
  val d2_emptyRow = (d2_currRowPtr === d2_rowPtr2Data)
  d2_endOfRow_q := d2_endOfRow
  val d2_emptyRow_q = RegNext(d2_emptyRow)
  d2_nextRowPtr_q := Mux(d2_endOfRow, d2_rowPtr1Data , d2_currRowPtr + 1.U)
  d2_nextValid := !d2_endOfRow && d2_valid && !d2_emptyRow
  d2_nextValid_q := d2_nextValid

  /* Pipeline Stage: DR (DataRead)
  Cycles = 1
  Inputs:
    1. colIdx, denCol  and peReq from D2 stage
    2. colIdx, denCol  and peReqfrom M stage
  Performs:
    1. Arbitrates between D1 stage and M stage. M stage is always given priority
    2. Reads the colIdx and sends the data to stage M
  Output:
    1. RowPtrData1, RowPtrData2 and goes to D2.
  */
  val dr_valid_q = RegNext(d2_valid)
  val dr_rowNum_q = RegEnable(d2_rowNum_q, dr_valid_q)
  val dr_colIdx = spCol.io.spReadData.data
  val dr_outWrite_q = RegEnable(d2_endOfRow_q, dr_valid_q)
  val dr_outWriteEmptyRow_q = RegEnable(d2_emptyRow_q, dr_valid_q)
  val dr_isNewOutput_q = RegEnable(d2_isNewOutput_q, dr_valid_q)
  val dr_isPR_q = RegEnable(d2_isPR_q, dr_valid_q)
  spVal.io.spReadCmd.addr := (d2_currRowPtr_q << log2Ceil(blockSizeBytes))
  val dr_denCol  = RegInit(VecInit(Seq.tabulate(cp.nPE)(n => n.U(M_SRAM_OFFSET_BITS.W)))) 
  for( i<-0 until cp.nPE){
    spDen.map(_.io.spReadCmd(i).addr).zip(0 to cp.nPE).map{case(spRd, idx) => spRd := ((dr_colIdx << log2Ceil(blockSizeBytes)) << log2Ceil(cp.nColInDense)) + (dr_denCol(i) << log2Ceil(blockSizeBytes))} 
  }

  val noOffsetRowAddress = d2_rowNum_q - 1.U

  val dr_emptyRowCount = RegInit(0.U(M_SRAM_OFFSET_BITS.W))
  
  when(pulse){
    dr_emptyRowCount := 0.U(M_SRAM_OFFSET_BITS.W)
  }.elsewhen(d2_valid){
    dr_emptyRowCount := dr_emptyRowCount + d2_emptyRow
  }.otherwise{
    dr_emptyRowCount := dr_emptyRowCount
  }
  

    /* Pipeline Stage: M (MAC)
  Cycles = 1
  */
  val m_isNewRow = dr_isNewOutput_q
  val m_acc_q = for(i <- 0 until cp.groupSize) yield {RegInit(VecInit(Seq.fill(cp.nColInDense)(0.U(cp.blockSize.W))))} 
  val m_valid_q = RegNext(dr_valid_q)
  val m_dense = spDen.map(_.io.spReadData.map(_.data))
  val m_sparse = spVal.io.spReadData.data
  val m_multiply = for(i <- 0 until cp.groupSize) yield {m_dense(i).map(_*m_sparse)}
  val m_mac = for(i <- 0 until cp.groupSize) yield {m_multiply(i).zip(m_acc_q(i)).map{case(x,y) => (Mux(dr_isNewOutput_q, x, x+y))}}
  val m_outWrite = dr_outWrite_q || dr_outWriteEmptyRow_q
  val writeResult = m_outWrite && m_valid_q
  when(m_valid_q){
    for(i <- 0 until cp.groupSize) yield {m_acc_q(i).zip(m_mac(i)).map{case(x_q, x) => x_q := x}}
  }.otherwise{
    for(i <- 0 until cp.groupSize) yield {m_acc_q(i).map{case(x_q) => x_q := x_q}}
  }

  val rowAddress = (dr_rowNum_q - 1.U + rowOffset) << log2Ceil((cp.blockSize * cp.nColInDense)/8)
  val formattedOutput = for(i <- 0 until cp.groupSize) yield {Mux(dr_outWriteEmptyRow_q, 0.U, m_mac(i).map(_(cp.blockSize-1, 0)).reverse.reduce(Cat(_,_)))}

  val writePartial1 = writeResult && dr_isPR_q && (dr_rowNum_q === 1.U)
  val writePartial2 = writeResult && dr_isPR_q && (dr_rowNum_q =/= 1.U)

  for(i <- 0 until cp.groupSize){
    io.partialRowWithPrev(i).data := RegEnable(formattedOutput(i), writePartial1)
    io.partialRowWithPrev(i).address := RegEnable(rowAddress, writePartial1)
    io.partialRowWithNext(i).data := RegEnable(formattedOutput(i), writePartial2)
  }

  // Debugging
  dontTouch(io.partialRowWithNext)
  dontTouch(io.partialRowWithPrev)
  

  // Output buffer
  val outBuff = for(i <- 0 until cp.groupSize) yield {Module(new Queue(new SPWriteCmd, cp.PEOutputBufferDepth))}

  io.outputQueueEmpty := ~(outBuff.map(_.io.deq.valid).reduce(_ || _))
  
  for(i <- 0 until cp.groupSize){
    outBuff(i).io.enq.bits.data := formattedOutput(i)
    outBuff(i).io.enq.bits.addr := rowAddress
    outBuff(i).io.enq.valid := writeResult && !dr_isPR_q
    assert(outBuff(i).io.enq.ready === true.B, "ERROR: Group #" + groupID.toString() + "output buffer full!")
    outBuff(i).io.deq <> io.outputBufferWriteData(i)
  }

  val pipeEmpty = (d1_state_q === sIdle) && (d1Queue.io.count === 0.U) && (!d2_valid) && (!dr_valid_q) && (!m_valid_q) && !(d1Queue.io.enq.valid)
  io.done := !pulse && pipeEmpty
}