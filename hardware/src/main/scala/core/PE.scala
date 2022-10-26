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
  val rowIdx = Input(UInt(C_XSIZE_BITS.W))
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

  val writeEnVec = Wire(Vec(cp.nScratchPadMem, Bool()))

  for(i <- 0 until cp.nScratchPadMem){
    io.spWrite(i).ready := true.B
    writeEnVec(i) := io.spWrite(i).fire
  }

  // Scratchpad Instantiation
  val spVal = Module(new Scratchpad(scratchType = "Val"))
  val spCol = Module(new Scratchpad(scratchType = "Col"))
  val spPtr = Module(new Scratchpad(scratchType = "Ptr"))
  val spDen = Module(new Scratchpad(scratchType = "Den"))
  io.spWrite(0).bits <> spVal.io.spWrite
  writeEnVec(0)      <> spVal.io.writeEn
  io.spWrite(1).bits <> spDen.io.spWrite
  writeEnVec(1)      <> spDen.io.writeEn
  io.spWrite(2).bits <> spPtr.io.spWrite
  writeEnVec(2)      <> spPtr.io.writeEn
  io.spWrite(3).bits <> spCol.io.spWrite
  writeEnVec(3)      <> spCol.io.writeEn

  val blockSizeBytes = (cp.blockSize/8)

  // Registers 
  val ptrCurr_q = RegInit(0.U(32.W))
  val colCurr_q = RegInit(0.U(32.W))
  val rowPtr1Data = Reg(UInt(32.W))
  val rowPtr2Data = Reg(UInt(32.W))
  val macCount = RegInit(0.U(32.W)) 
  val acc = RegInit(0.U(cp.blockSize.W))

  // state machine

  val sIdle :: sRowPtr1 :: sRowPtr2 :: sCol :: sMAC :: Nil = Enum(5)
  val endOfRow = ((state === sMAC) && (ptrCurr === rowPtr2Data))
  val denCol_q = RegInit(0.U(32.W))
  val denCol = Mux(endOfRow, denCol_q + 1.U, denCol_q)
  val endofCol = ((state === sMAC) && (denCol_q === peReq_q.denXSize))
  val state = RegInit(sIdle)
  val stateNext = RegInit(sIdle)
  val start = io.peReq.fire
  val peReq_q = RegEnable(io.peReq.bits, start)


  val rowPtrAddr = io.peReq.bits.sramPtr + ((io.peReq.bits.rowIdx) << log2Ceil(cp.blockSize/8))
  val rowPtrAddr_q = peReq_q.sramPtr + ((peReq_q.rowIdx) << log2Ceil(cp.blockSize/8))
  val rowPtrSPAddr = Mux(start,rowPtrAddr, rowPtrAddr_q)
  val rowPtrSPAddrNext = rowPtrSPAddr + (cp.blockSize/8).U
  val nonZeroInRow = Mux((state === sRowPtr2), spPtr.io.spReadData.data - rowPtr1Data, rowPtr2Data - rowPtr1Data)
  val done = ((state === sRowPtr2) && (nonZeroInRow === 0.U)
             || (endOfRow && endofCol))
  spPtr.io.spReadCmd.addr := Mux((state === sIdle) || done || (endOfRow && ! endofCol), rowPtrSPAddr, rowPtrSPAddrNext)

  val ptrCurr = Mux(state === sRowPtr1, spPtr.io.spReadData.data, ptrCurr_q)

  val colIdxAddr = peReq_q.sramColVal + (ptrCurr << log2Ceil(cp.blockSize/8))
  spCol.io.spReadCmd.addr := colIdxAddr
  val colCurr = Mux(state === sCol, spCol.io.spReadData.data, colCurr_q)

  val valAddr = io.peReq.bits.sramColVal + (ptrCurr << log2Ceil(cp.blockSize/8))
  spVal.io.spReadCmd.addr := valAddr
  val denAddr = io.peReq.bits.sramDen + ((colCurr << log2Ceil(cp.blockSize/8)) << Log2(peReq_q.denXSize)) + (denCol << log2Ceil(cp.blockSize/8))
  spDen.io.spReadCmd.addr := denAddr



  io.peReq.ready := (state === sIdle) || done

  switch(state){
    is(sIdle){
      denCol_q := 0.U
      when(start){
        state := sRowPtr1
      }
    }
    is(sRowPtr1){
      acc := 0.U
      state := sRowPtr2
      rowPtr1Data := spPtr.io.spReadData.data
      ptrCurr_q := spPtr.io.spReadData.data
    }
    is(sRowPtr2){
      rowPtr2Data := spPtr.io.spReadData.data
      when(nonZeroInRow === 0.U){
        when(start){
          acc := 0.U
          state := sRowPtr1
        }
      }.otherwise{
        state := sCol
      }
    }
    is(sCol){
      state := sMAC
      colCurr_q := spCol.io.spReadData.data
      ptrCurr := ptrCurr + 1.U
    }
    is(sMAC){
      acc :=  acc + (spVal.io.spReadData.data * spDen.io.spReadData.data)
      denCol_q := denCol_q + 1.U
      when(ptrCurr === rowPtr2Data){
        when(denCol_q === peReq_q.denXSize ){
          when(start){
            state := sRowPtr1
          }
        }.otherwise{
          state := sCol
          ptrCurr := rowPtr1Data
        }
      }.otherwise{
        state := sCol
      }
    }
  }

  assert(acc =/= 19.U)
}
