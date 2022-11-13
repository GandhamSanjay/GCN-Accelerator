package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._
import ISA._
import gcn.core.util.MuxTree

// class PECSRIO(implicit p: Parameters) extends Bundle{
//   val cp = p(AccKey).coreParams
//   val sramColVal = Input(UInt(C_SRAM_OFFSET_BITS.W))
//   val sramPtr = Input(UInt(C_SRAM_OFFSET_BITS.W))
//   val sramDen = Input(UInt(C_SRAM_OFFSET_BITS.W))
//   val denXSize = Input(UInt(C_XSIZE_BITS.W))
//   val spaYSize = Input(UInt(C_YSIZE_BITS.W))
//   val rowIdx = Input(UInt(C_XSIZE_BITS.W))
// }


// /* Group
//  */
class Group(val groupID: Int = 0)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val nBanks = cp.nColInDense
  val io = IO(new Bundle {
    val nRowPtrInGroup = Flipped(ValidIO(UInt(32.W)))
    val spWrite = Flipped(Decoupled(new SPWriteCmdWithSel))
    val mask = Input(UInt((nBanks).W))
    val nNonZero = Flipped(ValidIO(UInt(32.W)))
    val vrEntry = Decoupled(new VRTableEntry)
    // val start = Input(Bool())
  })
  val rowPtrSize = RegEnable(io.nRowPtrInGroup.bits, io.nRowPtrInGroup.valid)
  val nNonZero = RegEnable(io.nNonZero.bits, io.nNonZero.valid)
  val rowPtrBegin = RegInit(0.U(cp.blockSize.W))
  val rowPtrEnd = RegInit(0.U(cp.blockSize.W))
  when(io.nNonZero.valid){
    rowPtrBegin := rowPtrBegin + (groupID.U << Log2(io.nNonZero.bits))
    rowPtrEnd := rowPtrEnd + ((groupID + 1).U << Log2(io.nNonZero.bits))
  }
  val isVRwithPrevGroup_q = Reg(Bool())
  val numRows_q = Reg(UInt(cp.blockSize.W))
  val decompressDone = Wire(Bool())
  val vrQueue = Module(new Queue(new VRTableEntryWithGroup, 1)) 
  vrQueue.io.enq.valid := decompressDone
  vrQueue.io.enq.bits.VRTableEntry.isVRWithPrevGroup := isVRwithPrevGroup_q
  vrQueue.io.enq.bits.VRTableEntry.nRows := numRows_q
  vrQueue.io.enq.bits.group := groupID.U
  
  // ScratchPads
  val spVal = Module(new Scratchpad(scratchType = "Val", masked = true))
  val spCol = Module(new Scratchpad(scratchType = "Col", masked = true))
  val spPtr = Module(new Scratchpad(scratchType = "Ptr", masked = true))
  val spDen = Module(new BankedScratchpad(scratchType = "Den"))
  // val spOut = Module(new Scratchpad(scratchType = "Out", masked = false))

  io.spWrite.bits.spWriteCmd <> spVal.io.spWrite
  io.spWrite.bits.spWriteCmd <> spCol.io.spWrite
  io.spWrite.bits.spWriteCmd <> spDen.io.spWrite
  io.spWrite.bits.spWriteCmd <> spPtr.io.spWrite
  io.spWrite.ready := true.B
  spVal.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 0.U)
  spDen.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 1.U)
  spPtr.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 2.U)
  spCol.io.writeEn := io.spWrite.fire && (io.spWrite.bits.spSel === 3.U)

  spVal.io.spReadCmd.addr := io.spWrite.bits.spWriteCmd.addr
  spDen.io.spReadCmd.foreach{
    _.addr := 0.U
  }
  spPtr.io.spReadCmd.addr := io.spWrite.bits.spWriteCmd.addr
  spPtr.io.mask.get := io.mask
  spCol.io.spReadCmd.addr := io.spWrite.bits.spWriteCmd.addr
  spCol.io.mask.get := io.mask
  spVal.io.mask.get := io.mask

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

  // val d1_moving = Wire(Bool())
  // val sIdle :: sRowPtr1 :: sRowPtr2 :: Nil = Enum(3)
  // val d1_state_q = RegInit(sIdle)
  // val d1_ready = (d1_state_q === sIdle) || ((d1_state_q === sRowPtr2) && d1_moving)
  // val d1_isrowPtr1 = ((d1_state_q === sIdle) || ((d1_state_q === sRowPtr2) && d1_moving))
  // val d1_rowPtrAddr1 =  (io.peReq.bits.rowIdx << log2Ceil(cp.blockSize/8))
  // val d1_peReqNext_q = RegNext(d1_peReq_q)
  // val d1_rowPtrAddr2 =  ((d1_peReq_q.rowIdx + 1.U) << log2Ceil(blockSizeBytes)) 

  // switch(d1_state_q){
  //   is(sIdle){
  //     when(io.start){
  //       d1_state_q := sRowPtr1
  //     }
  //   }
  //   is(sRowPtr1){
  //     d1_state_q := sRowPtr2
  //   }
  //   is(sRowPtr2){
  //     when(d1_moving){
  //       when(io.peReq.valid){
  //         d1_state_q := sRowPtr1
  //       }.otherwise{
  //         d1_state_q := sIdle 
  //       }
  //     }
  //   }
  // }

  // spPtr.io.spReadCmd.addr  := Mux(d1_isrowPtr1, d1_rowPtrAddr1, d1_rowPtrAddr2)
  // val d1_rowPtr1Data_q = RegEnable(spPtr.io.spReadData.data, d1_state_q === sRowPtr1)
  // val d1_rowPtr2Data_q = RegEnable(spPtr.io.spReadData.data, d1_state_q === sRowPtr2)
  // val d1_rowPtr2Data = Mux(d1_state_q === sRowPtr2, spPtr.io.spReadData.data, d1_rowPtr2Data_q)
  // val d1_isRowEmpty = (d1_rowPtr2Data - d1_rowPtr1Data_q) === 0.U
  // val d1_valid = (d1_state_q === sRowPtr2) && !d1_isRowEmpty
  // val d1_currRowPtr = d1_rowPtr1Data_q
  // val d1_currDenCol = 0.U(cp.blockSize.W)

  // /* Pipeline Stage: D2
  // Cycles = 1
  // Inputs:
  //   1. rowPtr1Data, rowPtr2Data and (currRowPtr = rowPtr1Data), peReq from D1 stage
  //   2. rowPtr1Data, rowPtr2Data, updated currRowPtr, peReq from D2 stage
  // Performs:
  //   1. Arbitrates between requests from D1 stage and prev D2 stage. prev D2 stage is always given priority
  //   2. Reads the colIdx and sends the data to stage DR
  // Output:
  //   1. colIdx and peReq goes to DR.
  // */
  // val d2_nextValid = Wire(Bool())
  // val d2_nextValid_q = RegInit(false.B)
  // val d2_nextRowPtr_q = Reg(chiselTypeOf(d1_currRowPtr))
  // val d2_nextDenCol_q = Reg(chiselTypeOf(d1_currDenCol))
  // val d2_valid_q = RegInit(false.B)
  // val d2_rowPtr1Data_q = RegInit(0.U(cp.blockSize.W))
  // val d2_rowPtr2Data_q = RegInit(0.U(cp.blockSize.W))
  // val d2_currRowPtr_q = RegInit(0.U(cp.blockSize.W))
  // val d2_currDenCol_q = RegInit(0.U(cp.blockSize.W))
  // val d2_peReq_q = Reg(chiselTypeOf(d1_peReqNext_q))
  // val d2_peReq = Mux(d2_nextValid_q, d2_peReq_q, d1_peReqNext_q)
  // val d2_currRowPtr = Mux(d2_nextValid_q, d2_nextRowPtr_q, d1_currRowPtr)
  // val d2_currDenCol = Mux(d2_nextValid_q, d2_nextDenCol_q, d1_currDenCol)
  // val d2_rowPtr1Data = Mux(d2_nextValid_q, d2_rowPtr1Data_q, d1_rowPtr1Data_q)
  // val d2_rowPtr2Data = Mux(d2_nextValid_q, d2_rowPtr2Data_q, d1_rowPtr2Data_q)
  // val d2_endOfRow_q = RegInit(false.B)
  // val d2_endOfCol_q = RegInit(false.B)
  // val d2_isNewOutput_q = RegNext(d2_currRowPtr === d2_rowPtr1Data)
  // d1_moving := !d2_nextValid
  // d2_valid_q := d2_nextValid || d1_valid
  
  // d2_peReq_q := d2_peReq
  // d2_rowPtr1Data_q := d2_rowPtr1Data
  // d2_rowPtr2Data_q := d2_rowPtr2Data
  // d2_currRowPtr_q := d2_currRowPtr
  // d2_currDenCol_q := d2_currDenCol
  // spCol.io.spReadCmd.addr := d2_peReq.sramColVal + (d2_currRowPtr << log2Ceil(blockSizeBytes))
  // val d2_endOfCol = (d2_currDenCol === d2_peReq.denXSize - 1.U)
  // val d2_endOfRow = (d2_currRowPtr === d2_rowPtr2Data - 1.U)
  // d2_endOfRow_q := d2_endOfRow
  // d2_endOfCol_q := d2_endOfCol
  // val d2_endOfColRow = d2_endOfCol && d2_endOfRow
  // d2_nextDenCol_q := Mux(d2_endOfRow, d2_currDenCol + 1.U, d2_currDenCol)
  // d2_nextRowPtr_q := Mux(d2_endOfRow, d2_rowPtr1Data , d2_currRowPtr + 1.U)
  // d2_nextValid := !d2_endOfColRow && d2_valid_q
  // d2_nextValid_q := d2_nextValid

  // /* Pipeline Stage: DR (DataRead)
  // Cycles = 1
  // Inputs:
  //   1. colIdx, denCol  and peReq from D2 stage
  //   2. colIdx, denCol  and peReqfrom M stage
  // Performs:
  //   1. Arbitrates between D1 stage and M stage. M stage is always given priority
  //   2. Reads the colIdx and sends the data to stage M
  // Output:
  //   1. RowPtrData1, RowPtrData2 and goes to D2.
  // */
  // val dr_valid_q = RegNext(d2_valid_q)
  // val dr_colIdx = spCol.io.spReadData.data
  // val dr_peReqdenXSize_q = RegEnable(d2_peReq_q.denXSize, dr_valid_q)
  // val dr_currSpaRow_q = RegEnable(d2_peReq_q.rowIdx, dr_valid_q)
  // val dr_currDenCol_q = RegEnable(d2_currDenCol_q, dr_valid_q)
  // val dr_outWrite_q = RegEnable(d2_endOfRow_q, dr_valid_q)
  // val dr_isNewOutput_q = RegEnable(d2_isNewOutput_q, dr_valid_q)
  // spVal.io.spReadCmd.addr := d2_peReq_q.sramColVal + (d2_currRowPtr_q << log2Ceil(blockSizeBytes))
  // spDen.io.spReadCmd.addr := d2_peReq_q.sramDen + ((dr_colIdx << log2Ceil(blockSizeBytes)) << Log2(d2_peReq_q.denXSize)) + (d2_currDenCol_q << log2Ceil(blockSizeBytes))

}