package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._

/** Core.
 *
 * The core defines the current GCN Accelerator architecture by connecting memory and
 * compute modules together such as load/store and compute.
 *
 * Also, the core must be instantiated by a wrapper using the
 * Control Registers (CR) and the Memory Engine (ME) interfaces.
 */

class Core(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cr = new CRClient
    val me = new MEMaster
  })
  val fetch = Module(new Fetch)
  val start = Wire(Bool())
  start := io.cr.launch

  val sIdle :: sBusy :: sFinish :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val ctr = RegInit(0.U(1.W))

  // Fetch instructions (tasks) from memory (DRAM) into queues (SRAMs)
  fetch.io.launch := io.cr.launch
  fetch.io.ins_baddr := io.cr.vals(0)
  fetch.io.ins_count := io.cr.vals(1)

  // Read(rd) and write(wr) from/to memory (i.e. DRAM)
  io.cr.finish := (state === sFinish)
  io.me.wr(0).cmd.valid := false.B
  io.me.wr(0).cmd.bits.addr := 0.U
  io.me.wr(0).cmd.bits.len := 0.U
  io.me.wr(0).cmd.bits.tag := 0.U
  io.me.wr(0).data.valid := false.B
  io.me.wr(0).data.bits.data := 0.U
  io.me.wr(0).data.bits.strb := 0.U
  io.me.rd(0) <> fetch.io.me_rd
  // io.me.rd(0).cmd.valid := state === sBusy
  // io.me.rd(0).cmd.bits.addr := 0.U
  // io.me.rd(0).cmd.bits.len := 0.U
  // io.me.rd(0).cmd.bits.tag := 0.U
  // io.me.rd(0).data.ready := true.B

  switch(state){
    is(sIdle){
        when(start){
            state := sBusy
            ctr := ctr + 1.U
        }
    }
    is(sBusy){
        when(ctr === 0.U){
            state := sFinish
        }.otherwise{
            ctr := ctr + 1.U
        }
    }
    is(sFinish){
        state := sIdle
    }
  }

}
