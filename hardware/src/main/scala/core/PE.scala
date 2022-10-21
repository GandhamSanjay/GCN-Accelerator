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
  val rowIdx = Input(UInt(log2Ceil(cp.nPE).W))
}


// /** Processing Element.
//  *
//  * Takes instructions from fetch module. Schedules computation between PEs.
//  * Arbitrates communication betwen PE and scratchpads.
//  */
class PE(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val io = IO(new Bundle {
    val peIO = Decoupled(new PECSRIO)
    val spIO = Decoupled(new SPReadCmdWithSel)
  })

  // Registers 
  val spSel = Wire(UInt(cp.nScratchPadMem.W))
  spSel := MuxTree(state, Seq(4.U,2.U,3.U))

  // state machine
  val sIdle :: sReadData :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val start = io.peIO.fire
  io.peIO.ready := (state === sIdle)

  val rowPtrAddr = io.peIO.bits.sramPtr + ((io.peIO.bits.rowIdx) << log2Ceil(cp.blockSize/8))

  switch(state){
    is(sIdle){
      when(start){

      }
    }
  }
}
