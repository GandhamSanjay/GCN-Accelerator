package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._
import ISA._
import gcn.core.util.MuxTree

class PECSRIO(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val sramColVal = Input(UInt(C_SRAM_OFFSET_BITS.W))
  val sramPtr = Input(UInt(C_SRAM_OFFSET_BITS.W))
  val sramDen = Input(UInt(C_SRAM_OFFSET_BITS.W))
  val denXSize = Input(UInt(C_XSIZE_BITS.W))
  val rowIdx = Input(UInt(log2Ceil(cp.nPE).W))
}


// /** Processing Element.
//  *
//  * Takes instructions from fetch module. Schedules computation between PEs.
//  * Each PE instantiates each scratchpad buffer
//  */
class PECSR(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val io = IO(new Bundle {
    val peReq = Flipped(Decoupled(new PECSRIO))
    val spWrite = Vec(cp.nScratchPadMem, Flipped(Decoupled(new SPWriteCmd)))
  })

  val writeEnVec = Vec(cp.nScratchPadMem, Wire(Bool()))

  for(i <- 0 until cp.nScratchPadMem){
    io.spWrite(i).ready := true.B
    writeEnVec(i) := io.spWrite(i).fire
  }

  // Scratchpad Instantiation
  val spVal = Module(new Scratchpad(scratchType = "Val"))
  val spCol = Module(new Scratchpad(scratchType = "Col"))
  val spPtr = Module(new Scratchpad(scratchType = "Ptr"))
  val spDen = Module(new Scratchpad(scratchType = "Den"))
  io.spWrite(0)  <> spVal.io.spWrite
  writeEnVec(0)  <> spVal.io.writeEn
  io.spWrite(1)  <> spDen.io.spWrite
  writeEnVec(1)  <> spDen.io.writeEn
  io.spWrite(2)  <> spPtr.io.spWrite
  writeEnVec(2)  <> spPtr.io.writeEn
  io.spWrite(3)  <> spCol.io.spWrite
  writeEnVec(3)  <> spCol.io.writeEn

  val blockSizeBytes = (cp.blockSize/8)

  // Registers 
  val ptrCurr = RegInit(0.U(32.W))
  val colCurr_q = RegInit(0.U(32.W))

  val macCount = RegInit(0.U(32.W)) 
  val acc = RegInit(0.U(cp.blockSize.W))

  // state machine
  val sIdle :: sRowPtr1 :: sRowPtr2 :: sCol :: sValDen :: sMAC:: sOut :: Nil = Enum(8)
  val state = RegInit(sIdle)
  val stateNext = RegInit(sIdle)
  io.peReq.ready := (state === sIdle) || done
  val start = io.peReq.fire
  val peReq_q = RegEnable(io.peReq.bits, start)

  val rowPtrAddr = io.peReq.bits.sramPtr + ((io.peReq.bits.rowIdx) << log2Ceil(cp.blockSize/8))
  val rowPtrAddr_q = peReq_q.sramPtr + ((peReq_q.rowIdx) << log2Ceil(cp.blockSize/8))
  val rowPtrSPAddr = Mux(start,rowPtrAddr, rowPtrAddr_q)
  val rowPtrSPAddrNext = rowPtrSPAddr + 1.U
  spPtr.io.spReadCmd.addr := Mux((state === sIdle), rowPtrSPAddr, rowPtrSPAddrNext)

  val colIdxAddr = peReq_q.sramColVal + (ptrCurr << log2Ceil(cp.blockSize/8))
  spCol.io.spReadCmd.addr := colIdxAddr
  val colCurr = Mux(state === sCol, spCol.io.spReadData.data, colCurr_q)

  val valAddr = io.peReq.bits.sramColVal + (ptrCurr << log2Ceil(cp.blockSize/8))
  spVal.io.spReadCmd.addr := valAddr
  val denAddr = io.peReq.bits.sramDen + ((colCurr << log2Ceil(cp.blockSize/8)) << Log2(peReq_q.denXSize)) + (peReq_q.rowIdx << log2Ceil(cp.blockSize/8))
  spDen.io.spReadCmd.addr := denAddr

  val rowPtr1Data = Reg(UInt(32.W))
  val rowPtr2Data = Reg(UInt(32.W))
  val nonZeroInRow = Mux((state === sRowPtr2), spPtr.io.spReadData.data - rowPtr1Data, rowPtr2Data - rowPtr1Data)
  val done = ((state === sRowPtr2) && (nonZeroInRow === 0.U)
             || ((state === sMAC) && (ptrCurr === rowPtr2Data)))


  switch(state){
    is(sIdle){
      when(start){
        acc := 0.U
        state := sRowPtr1
      }
    }
    is(sRowPtr1){
      state := sRowPtr2
      rowPtr1Data := spPtr.io.spReadData.data
      ptrCurr := spPtr.io.spReadData.data
    }
    is(sRowPtr2){
      rowPtr2Data := spPtr.io.spReadData.data
      when(nonZeroInRow === 0.U){
        state := sIdle
      }.otherwise{
        state := sCol
      }
    }
    is(sCol){
      state := sMAC
      colCurr := spCol.io.spReadData.data
      state := sValDen
    }
    is(sValDen){
      ptrCurr := ptrCurr + 1.U
    }
    is(sMAC){
      acc :=  acc + (spVal.io.spReadData.data * spDen.io.spReadData.data)
      when(ptrCurr === rowPtr2Data){
        state := sIdle
      }.otherwise{
        state := sCol
      }
    }
  }
}
