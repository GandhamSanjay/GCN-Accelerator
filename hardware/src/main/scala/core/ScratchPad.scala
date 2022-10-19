package gcn.core

import scala.math.pow

import chisel3._
import chisel3.util._
import vta.util.config._
import os.write


/** Scratchpad Logic.
 *
 * Load 1D and 2D tensors from main memory (DRAM) to input/weight
 * scratchpads (SRAM). Also, there is support for zero padding, while
 * doing the load. Zero-padding works on the y and x axis, and it is
 * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
 * handling the way tensors are stored on the scratchpads.
 * 
 */

class SPWriteCmd(implicit p: Parameters) extends Bundle{
  val M_SRAM_OFFSET_BITS = 16
  val cp = p(AccKey).coreParams
  val mp = p(AccKey).memParams
  val addr = UInt(M_SRAM_OFFSET_BITS.W)
  val data = UInt(mp.dataBits.W)
  val spSel = UInt(cp.nScratchPadMem.W)
}
 
class Scratchpad(scratchType: String = "Col", debug: Boolean = false)(implicit p: Parameters)extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val io = IO(new Bundle {
    val spWrite = Flipped(Decoupled(new SPWriteCmd))
    val out = Output(Bool())
  })

  val write_q = Module(new Queue(new SPWriteCmd, 10))
  write_q.io.enq <> io.spWrite
  io.out := (write_q.io.count === 0.U)
  write_q.io.deq.ready := false.B

}

