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
  val cp = p(AccKey).coreParams
  val fetch = Module(new Fetch)
  val load = Module(new Load)
  val compute = Module(new Compute)
  val start = Wire(Bool())

  compute.io.spWrite <> load.io.spWrite

  start := io.cr.launch

  val sIdle :: sLoad :: sCompute :: sFinish :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val ctr = RegInit(0.U(3.W))
  compute.io.valid := (state === sCompute) && !compute.io.done
  load.io.valid := (state === sLoad) && !load.io.done

  // Fetch instructions (tasks) from memory (DRAM) into queues (SRAMs)
  fetch.io.launch := io.cr.launch
  fetch.io.ins_baddr := io.cr.vals(0)
  fetch.io.ins_count := io.cr.vals(1)

  // Load inputs and weights from memory (DRAM) into scratchpads (SRAMs)
  load.io.inst <> fetch.io.inst.ld
  compute.io.inst <> fetch.io.inst.co

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
  io.me.rd(1) <> load.io.me_rd
  // io.me.rd(0).cmd.valid := state === sBusy
  // io.me.rd(0).cmd.bits.addr := 0.U
  // io.me.rd(0).cmd.bits.len := 0.U
  // io.me.rd(0).cmd.bits.tag := 0.U
  // io.me.rd(0).data.ready := true.B

  switch(state){
    is(sIdle){
        when(start){
            state := sLoad
            ctr := ctr + 1.U
        }
    }
    is(sLoad){
      when(load.io.done){
        when(ctr === 4.U){
          state := sCompute
        }.otherwise{
          state := sLoad
          ctr := ctr + 1.U
        }
      }
    }
    is(sCompute){
      when(compute.io.done){
        state := sLoad
        ctr := 0.U
      }
    }
    is(sFinish){
        state := sIdle
    }
  }

}
