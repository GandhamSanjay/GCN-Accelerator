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
  val cr = p(AccKey).crParams
  val fetch = Module(new Fetch)
  val load = Module(new Load)
  val compute = Module(new Compute)
  val store = Module(new Store)
  val spOut = Module(new OutputScratchpad(scratchType = "Out"))
  val start = Wire(Bool())

  spOut.io.spWrite <> compute.io.spOutWrite.bits
  compute.io.spOutWrite.ready := true.B
  spOut.io.writeEn := compute.io.spOutWrite.valid
  compute.io.spWrite <> load.io.spWrite
  spOut.io.spReadCmd.addr := 0.U
  store.io.spReadCmd <> spOut.io.spReadCmd
  store.io.spReadData <> spOut.io.spReadData

  io.cr.ecnt(0) <> load.io.ecnt
  io.cr.ecnt(1) <> compute.io.ecnt(0)
  io.cr.ecnt(2) <> store.io.ecnt
  for(i <- 0 until cp.nPE){
    for(j <- 0 until cr.nPEEventCtr){
      io.cr.ecnt(3+(i*cr.nPEEventCtr) + j) <> compute.io.ecnt((i*cr.nPEEventCtr) + j + 1)
 }
  }
 
  start := io.cr.launch

  val sIdle :: sLoad :: sCompute :: sStore :: sFinish :: Nil = Enum(5)
  val state = RegInit(sIdle)
  val ctr = RegInit(0.U(3.W))
  compute.io.valid := (state === sCompute) && !compute.io.done
  load.io.valid := (state === sLoad) && !load.io.done
  store.io.valid := (state === sStore) && !store.io.done

  // Fetch instructions (tasks) from memory (DRAM) into queues (SRAMs)
  fetch.io.launch := io.cr.launch
  fetch.io.ins_baddr := io.cr.vals(0)
  fetch.io.ins_count := io.cr.vals(1)

  // Load inputs and weights from memory (DRAM) into scratchpads (SRAMs)
  load.io.inst <> fetch.io.inst.ld
  compute.io.inst <> fetch.io.inst.co
  store.io.inst <> fetch.io.inst.st

  // Read(rd) and write(wr) from/to memory (i.e. DRAM)
  io.cr.finish := (state === sFinish)
  io.me.rd(0) <> fetch.io.me_rd
  io.me.rd(1) <> load.io.me_rd
  io.me.wr(0) <> store.io.me_wr

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
        state := sStore
        ctr := 0.U
      }
    }
    is(sStore){
      when(store.io.done){
        state := sFinish
      }
    }
    is(sFinish){
        state := sIdle
    }
  }

}
