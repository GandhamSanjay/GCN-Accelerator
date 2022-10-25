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
  val readCmdVec = Vec(cp.nScratchPadMem, new SPReadCmd)
  val readDataVec = Vec(cp.nScratchPadMem, new SPReadData)

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
  readCmdVec(0)  <> spVal.io.spReadCmd
  readDataVec(0) <> spVal.io.spReadData
  writeEnVec(0)  <> spVal.io.writeEn
  io.spWrite(1)  <> spDen.io.spWrite
  readCmdVec(1)  <> spDen.io.spReadCmd
  readDataVec(1) <> spDen.io.spReadData
  writeEnVec(1)  <> spDen.io.writeEn
  io.spWrite(2)  <> spPtr.io.spWrite
  readCmdVec(2)  <> spPtr.io.spReadCmd
  readDataVec(2) <> spPtr.io.spReadData
  writeEnVec(2)  <> spPtr.io.writeEn
  io.spWrite(3)  <> spCol.io.spWrite
  readCmdVec(3)  <> spCol.io.spReadCmd
  readDataVec(3) <> spCol.io.spReadData
  writeEnVec(3)  <> spCol.io.writeEn

  val blockSizeBytes = (cp.blockSize/8)

  // Registers 
  val sSel = RegInit(0.U(cp.nScratchPadMem.W))
  val sAddr = RegInit(0.U(C_SRAM_OFFSET_BITS.W))
  val sValid = Reg(Bool())
  val ptrStartIdx = RegInit(0.U(32.W))
  val ptrEndIdx = RegInit(0.U(32.W))
  val ptrCurr = RegInit(0.U(32.W))
  val valCurr = RegInit(0.U(cp.blockSize.W))
  val colCurr = RegInit(0.U(32.W))
  val macCount = RegInit(0.U(32.W)) 
  val acc = RegInit(0.U(cp.blockSize.W))
  val denCol = RegInit(0.U(32.W))

  // state machine
  val sIdle :: sRowPtr1 :: sRowPtr2 :: sCol :: sMAC:: sOut :: Nil = Enum(8)
  val state = RegInit(sIdle)
  val stateNext = RegInit(sIdle)
  io.peReq.ready := (state === sIdle)
  val start = io.peReq.fire
  val peReq_q = RegEnable(io.peReq.bits, start)

  val rowPtrAddr = io.peReq.bits.sramPtr + ((io.peReq.bits.rowIdx) << log2Ceil(cp.blockSize/8))
  val rowPtrAddr_q = peReq_q.sramPtr + ((peReq_q.rowIdx) << log2Ceil(cp.blockSize/8))

  val rowPtrSPAddr = Mux(start,rowPtrAddr, rowPtrAddr_q)
  val rowPtrSPAddrNext = rowPtrSPAddr + 1.U
  val rowPtr1Data = Reg(UInt(32.W))
  val rowPtr2Data = Reg(UInt(32.W))
  val nonZeroInRow = Mux((state === sRowPtr2), spPtr.io.spReadData.data - rowPtr1Data, rowPtr2Data - rowPtr1Data)

  val SPValid = Wire(Bool())
  val SPAddr = Wire(chiselTypeOf((new SPReadCmd).addr))
  SPValid := ((state === sIdle) && start)
  SPAddr := (state === sIdle) && start

  switch(state){
    is(sIdle){
      when(start){
        state := sRowPtr1
      }
    }
    is(sRowPtr1){
      state := sRowPtr2
      rowPtr1Data := spPtr.io.spReadData.data
    }
    is(sRowPtr2){
      state := sCol
      rowPtr2Data := spPtr.io.spReadData.data
    }
    is(sCol){
      state := sMAC
    }
    is(sMAC){
      state := sOut
    }
    is(sOut){
      state := sIdle
    }
  }
}
