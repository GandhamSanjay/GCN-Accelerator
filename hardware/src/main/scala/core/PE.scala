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
//  * Arbitrates communication betwen PE and scratchpads.
//  */
class PECSR(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val io = IO(new Bundle {
    val peReq = Flipped(Decoupled(new PECSRIO))
    val spReadCmd = Decoupled(new SPReadCmdWithSel)
    val spData = Flipped(Decoupled(new SPReadData))
  })


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
  val nonZero = ptrEndIdx - ptrStartIdx
  val acc = RegInit(0.U(cp.blockSize.W))
  val denCol = RegInit(0.U(32.W))

  // state machine
  val sIdle :: sReadCmd :: sRowPtr :: sRowPtr2 :: sVal :: sCol :: sDen ::sDone :: Nil = Enum(8)
  val state = RegInit(sIdle)
  val stateNext = RegInit(sIdle)
  io.peReq.ready := (state === sIdle)
  val start = io.peReq.fire
  val peReq_q = RegEnable(io.peReq.bits, start)

  val rowPtrAddr = io.peReq.bits.sramPtr + ((io.peReq.bits.rowIdx) << log2Ceil(cp.blockSize/8))
  val rowPtrAddr_q = peReq_q.sramPtr + ((peReq_q.rowIdx) << log2Ceil(cp.blockSize/8))

  switch(state){
    is(sIdle){
      when(start){
        acc := 0.U
        sAddr := rowPtrAddr
        sSel := scratchID("Ptr").U
        state := sReadCmd
        stateNext := sRowPtr
      }.otherwise{
        sValid := false.B
        state := sIdle
      }
    }
    is(sReadCmd){
      when(io.spReadCmd.ready){
        state := stateNext
      }
    }
    is(sRowPtr){
      when(io.spData.fire){
        ptrStartIdx := io.spData.bits.data
        sAddr := rowPtrAddr_q + blockSizeBytes.U
        sSel := scratchID("Ptr").U
        state := sReadCmd
        stateNext := sRowPtr2
      }
    }
    is(sRowPtr2){
      when(io.spData.fire){
        ptrEndIdx := io.spData.bits.data
        when((io.spData.bits.data - ptrStartIdx) === 0.U){
          state := sIdle
        }.otherwise{
          sAddr := peReq_q.sramColVal + (ptrStartIdx << log2Ceil(blockSizeBytes))
          ptrCurr := ptrStartIdx
          sSel := scratchID("Val").U
          state := sReadCmd
          stateNext := sVal
        }
      }
    }
    is(sVal){
      when(io.spData.fire){
        valCurr := io.spData.bits.data
        sSel := scratchID("Col").U
        state := sReadCmd
        stateNext := sCol
      }
    }
    is(sCol){
      when(io.spData.fire){
        sAddr := peReq_q.sramDen + (((io.spData.bits.data << (Log2(peReq_q.denXSize))) + denCol) << log2Ceil(blockSizeBytes))
        colCurr := io.spData.bits.data
        sSel := scratchID("Den").U
        state := sReadCmd
        stateNext := sDen
        ptrCurr := ptrCurr + 1.U
      }
    }
    is(sDen){
      when(io.spData.fire){
        acc := acc + (io.spData.bits.data * valCurr)
        when(ptrCurr === ptrEndIdx){
          when(denCol === peReq_q.denXSize - 1.U){
            state := sIdle
          }.otherwise{
            acc := 0.U
            denCol := denCol + 1.U
            sAddr := peReq_q.sramColVal + (ptrStartIdx << log2Ceil(blockSizeBytes))
            ptrCurr := ptrStartIdx
            sSel := scratchID("Val").U
            state := sReadCmd
            stateNext := sVal
          }
          
        }.otherwise{
          sAddr := peReq_q.sramColVal + (ptrCurr << log2Ceil(blockSizeBytes))
          sSel := scratchID("Val").U
          state := sReadCmd
          stateNext := sVal
        }
      }
    }
  }

  assert(acc =/= (1<<20).U)

  io.spReadCmd.bits.spReadCmd.addr := sAddr
  io.spReadCmd.bits.spSel := sSel 
  io.spReadCmd.valid := (state === sReadCmd)
  io.spData.ready := true.B
  io.spReadCmd.bits.spReadCmd.tag := 0.U
}
