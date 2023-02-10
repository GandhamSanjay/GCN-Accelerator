package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._
import ISA._
import gcn.core.util.MuxTree

class colPtrData(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val colPtr1Data = UInt(32.W)
  val colPtr2Data = UInt(32.W)
}


// /* Group
//  */
class GroupCSC(val groupID: Int = 0)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val nBanks = cp.nColInDense
  val io = IO(new Bundle {
    val nColPtrInGroup = Flipped(ValidIO(UInt(32.W)))
    val spWrite = Flipped(Decoupled(new SPWriteCmdWithSel))
    val ptrSpWrite = Flipped(Decoupled(new SPWriteCmd(mode = "single")))
    val nNonZero = Flipped(ValidIO(UInt(32.W)))
    val vcEntry = Decoupled(new VCTableEntry)
    val outReadCmd = Input(Vec(cp.nColInDense, new SPReadCmd))
    val outReadData = Output(Vec(cp.nColInDense, new SPReadData))
    val colPtrOffset = Flipped(ValidIO(UInt(32.W)))
    val start = Input(Bool())
    val done = Output(Bool())
  })
  
  val pulse = io.start && !RegNext(io.start)
  val colPtrSize = RegEnable(io.nColPtrInGroup.bits, io.nColPtrInGroup.valid)
  val nNonZero = RegEnable(io.nNonZero.bits, io.nNonZero.valid)
  val colPtrBegin = RegInit(0.U(cp.blockSize.W))
  val colPtrEnd = RegInit(0.U(cp.blockSize.W))
  val totalNonZero = RegInit(0.U(cp.blockSize.W))
  val colPtrOffset = RegEnable(io.colPtrOffset.bits, io.colPtrOffset.valid)

  when(io.nNonZero.valid){
    // Find the bounds of this group's segment of the col pointer
    // If nNonZero = 2, group 0 gets [0,2) + offset, group 1 gets [2,4) + offset, etc
    // Used to remove the offset and determine if virtual col pointers are neccessary
    colPtrBegin := colPtrBegin + (groupID.U << Log2(io.nNonZero.bits))
    colPtrEnd := colPtrEnd + ((groupID + 1).U << Log2(io.nNonZero.bits))
  }.elsewhen(io.done && !RegNext(io.done)){ // Rising edge
    totalNonZero := totalNonZero + (io.nNonZero.bits << log2Ceil(cp.nGroups))
    colPtrBegin := totalNonZero + (io.nNonZero.bits << log2Ceil(cp.nGroups))
    colPtrEnd := totalNonZero + (io.nNonZero.bits << log2Ceil(cp.nGroups))
  }
  val d1_colPtrAddr = Wire(UInt(32.W))
  val vcQueue = Module(new Queue(new VCTableEntry, 1)) 
  vcQueue.io.deq <> io.vcEntry
  
  // ScratchPads
  val spVal = Module(new Scratchpad(scratchType = "Val", masked = false))
  val spRow = Module(new Scratchpad(scratchType = "Row", masked = false))
  val spPtr = Module(new SingleScratchpad(scratchType = "Ptr", masked = false))
  val spDen = Module(new BankedScratchpad(scratchType = "Den"))
  val spOut = Module(new BankedScratchpad(scratchType = "Out"))

  io.spWrite.bits.spWriteCmd <> spVal.io.spWrite
  io.spWrite.bits.spWriteCmd <> spRow.io.spWrite
  io.spWrite.bits.spWriteCmd <> spDen.io.spWrite
  io.ptrSpWrite.bits <> spPtr.io.spWrite
  io.spWrite.ready := true.B
  io.ptrSpWrite.ready := true.B
  spVal.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 0.U)
  spDen.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 1.U)
  spPtr.io.writeEn := io.ptrSpWrite.fire
  spRow.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 3.U)

  spVal.io.spReadCmd.addr := io.spWrite.bits.spWriteCmd.addr
  spPtr.io.spReadCmd.addr := d1_colPtrAddr
  spRow.io.spReadCmd.addr := io.spWrite.bits.spWriteCmd.addr

  val blockSizeBytes = (cp.blockSize/8)

  /* Pipeline Stage: D1
  Cycles = 2
  Inputs:
    1. Col assignment from Compute module via peReq
  Performs:
    1. Uses a statemachine to read two consecutive addresses in col_ptr starting from assigned col
    2. Sends colPtrData1, colPtrData2 to next pipeline stage D2
  Output:
    1. colPtrData1, colPtrData2, (currColPtr = colPtr1Data) and peReq goes to D2.
  */
  val d1Queue = Module(new Queue(new colPtrData(), 15))
  val d1_reqValid_q = Reg(Bool())
  val sIdle :: sColPtr1 :: sColPtr2  :: Nil = Enum(3)
  val d1_state_q = RegInit(sIdle)
  val d1_statePrev_q = RegNext(d1_state_q)
  val d1_colPtrAddr_q = RegInit(0.U(M_SRAM_OFFSET_BITS.W))
  val d1_colPtrInc = Wire(Bool())
  d1_colPtrAddr := Mux(d1_colPtrInc, d1_colPtrAddr_q + blockSizeBytes.U, d1_colPtrAddr_q) 
  val d1_colPtr1Data_q = Reg(chiselTypeOf(spPtr.io.spReadData.data))
  val d1_colPtr2Data_q = Reg(chiselTypeOf(spPtr.io.spReadData.data))
  dontTouch(d1_colPtr1Data_q)
  val isVC = (spPtr.io.spReadData.data =/= colPtrBegin)
  val isVC_q = RegEnable(isVC, pulse)
  val d1_numColPtr_q = RegInit(0.U(32.W))
  val colPtrDone = d1_numColPtr_q >= colPtrSize
  d1Queue.io.enq.bits.colPtr1Data := d1_colPtr1Data_q
  d1Queue.io.enq.bits.colPtr2Data := d1_colPtr2Data_q
  d1Queue.io.enq.valid := d1_reqValid_q

  when((d1_state_q === sColPtr2) && !colPtrDone){
    d1_numColPtr_q := d1_numColPtr_q + 2.U
  }.elsewhen(d1_state_q === sIdle){
    d1_numColPtr_q := 0.U
  }

  // Increment col pointer to next block
  when((d1_state_q === sColPtr1) || (d1_state_q === sColPtr2)){
    d1_colPtrAddr_q := d1_colPtrAddr
    d1_colPtrInc := true.B
  }.otherwise{
    d1_colPtrInc := false.B
  }

  switch(d1_state_q){
    is(sIdle){
      d1_colPtrAddr_q := 0.U
      when(pulse){
        when(colPtrSize === 0.U){
          // Virtual col pointer on both ends
          d1_colPtr1Data_q := colPtrBegin - colPtrBegin
          d1_colPtr2Data_q := colPtrEnd - colPtrBegin
          d1_reqValid_q := true.B
        }.otherwise{
          when(isVC){
            // Virtual col pointer at start, continue normally
            d1_colPtr1Data_q := colPtrBegin - colPtrBegin
            d1_colPtr2Data_q := spPtr.io.spReadData.data - colPtrBegin
            d1_reqValid_q := true.B
            d1_state_q := sColPtr1
          }.otherwise{
            d1_state_q := sColPtr1
          }
        }
      }.otherwise{
        d1_reqValid_q := false.B
      }
    }
    is(sColPtr1){
      when(colPtrSize === 1.U){
        // Needs virtual col pointer on the end of the col
        d1_colPtr1Data_q := spPtr.io.spReadData.data - colPtrBegin
        d1_colPtr2Data_q := colPtrEnd - colPtrBegin
        d1_state_q := sIdle
        d1_reqValid_q := true.B
      }.otherwise{
        d1_reqValid_q := false.B
        d1_state_q := sColPtr2
        d1_colPtr1Data_q := spPtr.io.spReadData.data - colPtrBegin
      }
    }
    is(sColPtr2){
      when(colPtrDone){
        when(d1_colPtr2Data_q =/= (colPtrEnd - colPtrBegin)){
          // Needs virtual col pointer on the end of the col
          d1_colPtr1Data_q := d1_colPtr2Data_q
          d1_colPtr2Data_q := colPtrEnd - colPtrBegin
          d1_state_q := sIdle
          d1_reqValid_q := true.B
        }.otherwise{
          d1_state_q := sIdle
          d1_reqValid_q := false.B
        }
      }.otherwise{
        when(d1_statePrev_q === d1_state_q){
          d1_colPtr1Data_q := d1_colPtr2Data_q
        }
        d1_colPtr2Data_q := spPtr.io.spReadData.data - colPtrBegin
        d1_reqValid_q := true.B
        d1_state_q := sColPtr2
      }
    }
  }

  val d1_numCol_q = RegInit(0.U(32.W))
  when(d1Queue.io.enq.fire){
    d1_numCol_q := d1_numCol_q + 1.U
  }.elsewhen(pulse){
    d1_numCol_q := 0.U
  }
  val d1_numCol = Mux(d1Queue.io.enq.fire, d1_numCol_q + 1.U, d1_numCol_q)

  spPtr.io.spReadCmd.addr  := Mux(d1_state_q === sIdle, d1_colPtrAddr_q, d1_colPtrAddr)
  val d1_currColPtr = d1Queue.io.deq.bits.colPtr1Data
  val d1_currDenCol = 0.U(cp.blockSize.W)
  val decompressDone = ((d1_state_q === sIdle) && ((d1_statePrev_q === sColPtr2) || ((pulse) && colPtrSize === 0.U) || (d1_statePrev_q === sColPtr1)))
  vcQueue.io.enq.valid := RegNext(decompressDone)
  vcQueue.io.enq.bits.isVCWithPrevGroup := isVC_q
  vcQueue.io.enq.bits.nCols := d1_numCol

  /* Pipeline Stage: D2
  Cycles = 1
  Inputs:
    1. colPtr1Data, colPtr2Data and (currColPtr = colPtr1Data), peReq from D1 stage
    2. colPtr1Data, colPtr2Data, updated currColPtr, peReq from D2 stage
  Performs:
    1. Arbitrates between requests from D1 stage and prev D2 stage. prev D2 stage is always given priority
    2. Reads the rowIdx and sends the data to stage DR
  Output:
    1. rowIdx and peReq goes to DR.
  */
  val d2_colNum_q = RegInit(0.U(M_SRAM_OFFSET_BITS.W))
  when(d1Queue.io.deq.fire){
    d2_colNum_q := d2_colNum_q + 1.U
  }.elsewhen(pulse){
    d2_colNum_q := 0.U
  }
  val d2_nextValid = Wire(Bool())
  val d2_nextValid_q = RegInit(false.B)
  val d2_nextColPtr_q = Reg(chiselTypeOf(d1_currColPtr))
  val d2_valid = WireDefault(false.B)
  val d2_colPtr1Data_q = RegInit(0.U(cp.blockSize.W))
  val d2_colPtr2Data_q = RegInit(0.U(cp.blockSize.W))
  val d2_currColPtr_q = RegInit(0.U(cp.blockSize.W))
  val d2_currColPtr = Mux(d2_nextValid_q, d2_nextColPtr_q, d1_currColPtr)
  val d2_colPtr1Data = Mux(d2_nextValid_q, d2_colPtr1Data_q, d1Queue.io.deq.bits.colPtr1Data)
  val d2_colPtr2Data = Mux(d2_nextValid_q, d2_colPtr2Data_q, d1Queue.io.deq.bits.colPtr2Data)
  val d2_endOfCol_q = RegInit(false.B)
  val d2_isNewOutput_q = RegNext(d2_currColPtr === d2_colPtr1Data)
  d1Queue.io.deq.ready := !d2_nextValid
  val d1_valid = d1Queue.io.deq.valid
  d2_valid := d2_nextValid_q || (d1Queue.io.deq.valid && !d2_nextValid_q)
  d2_colPtr1Data_q := d2_colPtr1Data
  d2_colPtr2Data_q := d2_colPtr2Data
  d2_currColPtr_q := d2_currColPtr
  //spRow.io.spReadCmd.addr := (d2_currColPtr << log2Ceil(blockSizeBytes))
  val d2_endOfCol = (d2_currColPtr === (d2_colPtr2Data - 1.U))
  val d2_emptyCol = (d2_currColPtr === d2_colPtr2Data)
  d2_endOfCol_q := d2_endOfCol
  val d2_emptyCol_q = RegNext(d2_emptyCol)
  d2_nextColPtr_q := Mux(d2_endOfCol, d2_colPtr1Data , d2_currColPtr + 1.U)
  d2_nextValid := !d2_endOfCol && d2_valid && !d2_emptyCol
  d2_nextValid_q := d2_nextValid

  /* Pipeline Stage: DR (DataRead)
  Cycles = 1
  Inputs:
    1. rowIdx, denCol  and peReq from D2 stage
    2. rowIdx, denCol  and peReqfrom M stage
  Performs:
    1. Arbitrates between D1 stage and M stage. M stage is always given priority
    2. Reads the rowIdx and sends the data to stage M
  Output:
    1. ColPtrData1, ColPtrData2 and goes to D2.
  */
  val dr_valid_q = RegNext(d2_valid)
  val dr_colNum_q = RegEnable(d2_colNum_q, dr_valid_q)

  // In the CSR version we accessed a dense row based on the column index. We still need to do that,
  // but the column index is now based on the pointer accesses instead of being directly read from an array
  // The colPtrOffset accounts for the parts of the colPtr that are in other groups, and for the virtual column pointers
  val dr_rowIdx = RegNext(d2_colNum_q + colPtrOffset)
  val dr_outWrite_q = RegEnable(d2_endOfCol_q, dr_valid_q)
  val dr_outWriteEmptyCol_q = RegEnable(d2_emptyCol_q, dr_valid_q)
  val dr_isNewOutput_q = RegEnable(d2_isNewOutput_q, dr_valid_q)
  spVal.io.spReadCmd.addr := (d2_currColPtr_q << log2Ceil(blockSizeBytes))
  val dr_denCol  = RegInit(VecInit(Seq.tabulate(cp.nPE)(n => n.U(M_SRAM_OFFSET_BITS.W)))) 
  for( i<-0 until cp.nPE){
    spDen.io.spReadCmd(i).addr := ((dr_rowIdx << log2Ceil(blockSizeBytes)) << log2Ceil(cp.nColInDense)) + (dr_denCol(i) << log2Ceil(blockSizeBytes))
  }
  

    /* Pipeline Stage: M (MAC)
  Cycles = 1
  */
  val m_isNewCol = dr_isNewOutput_q
  val m_acc_q = RegInit(VecInit(Seq.fill(cp.nColInDense)(0.U(cp.blockSize.W)))) 
  val m_valid_q = RegNext(dr_valid_q)
  val m_dense = spDen.io.spReadData.map(_.data)
  val m_sparse = spVal.io.spReadData.data
  val m_multiply = m_dense.map(_*m_sparse)
  //val m_mac = m_multiply.zip(m_acc_q).map{case(x,y) => (Mux(dr_isNewOutput_q, x, x+y))}
  val m_mac = m_multiply
  val m_outWrite = dr_outWrite_q || dr_outWriteEmptyCol_q
  spOut.io.spWrite.data := Mux(dr_outWriteEmptyCol_q, 0.U, m_mac.map(_(cp.blockSize-1, 0)).reverse.reduce(Cat(_,_)))
  spOut.io.writeEn := m_outWrite && m_valid_q
  spOut.io.spWrite.addr := (((dr_colNum_q - 1.U) << log2Ceil(cp.blockSize/8))) << log2Ceil(cp.nColInDense)
  when(m_valid_q){
    m_acc_q.zip(m_mac).map{case(x_q, x) => x_q := x}
  }.otherwise{
    m_acc_q.map{case(x_q) => x_q := x_q}
  }

  spOut.io.spReadCmd <> io.outReadCmd
  spOut.io.spReadData <> io.outReadData
  val pipeEmpty = (d1_state_q === sIdle) && (d1Queue.io.count === 0.U) && (!d2_valid) && (!dr_valid_q) && (!m_valid_q) && !(d1Queue.io.enq.valid)
  io.done := !pulse && pipeEmpty
}