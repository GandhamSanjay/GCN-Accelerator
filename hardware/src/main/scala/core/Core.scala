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
  val globalBuffer = Module(new GlobalBuffer())
  val outputScratchpad = for(i <- 0 until cp.groupSize) yield{Module(new OutputScratchpad())}
  val compute = Module(new Compute)
  val store = Module(new Store)
  val start = Wire(Bool())

  load.io.spWrite.ready := true.B
  globalBuffer.io.spWrite <> load.io.spWrite.bits
  globalBuffer.io.writeEn := load.io.spWrite.fire && ~load.io.isDenseLoad
  globalBuffer.io.spReadCmd <> compute.io.gbReadCmd
  globalBuffer.io.spReadData <> compute.io.gbReadData
  compute.io.spOutWrite.zip(outputScratchpad).map{case(x,y) => x.bits <> y.io.spWrite}
  compute.io.spOutWrite.map{_.ready := true.B}
  compute.io.spOutWrite.zip(outputScratchpad).map{case(x,y) => y.io.writeEn := x.valid}
  compute.io.denWrite := load.io.spWrite.bits
  compute.io.denWriteEn := load.io.spWrite.fire && load.io.isDenseLoad
  compute.io.denseGroup := load.io.denseGroup

  io.cr.ecnt(0) := 0.U
  // io.cr.ecnt(0) <> load.io.ecnt
  // io.cr.ecnt(1) <> compute.io.ecnt(0)
  // io.cr.ecnt(2) <> store.io.ecnt
  // for(i <- 0 until cp.nPE){
  //   for(j <- 0 until cr.nPEEventCtr){
  //     io.cr.ecnt(3+(i*cr.nPEEventCtr) + j) <> compute.io.ecnt((i*cr.nPEEventCtr) + j + 1)
  //   }
  // }
 
  start := io.cr.launch

  val sIdle :: sLoad :: sCompute :: sStore :: sFinish :: Nil = Enum(5)
  val state = RegInit(sIdle)
  val ctr = RegInit(0.U(4.W))
  compute.io.valid := (state === sCompute) && !compute.io.done
  load.io.valid := (state === sLoad) && !load.io.done
  store.io.valid := (state === sStore) && !store.io.done

  // Fetch instructions (tasks) from memory (DRAM) into queues (SRAMs)
  fetch.io.launch := io.cr.launch
  fetch.io.ins_baddr := Cat(io.cr.vals(0),io.cr.vals(0))
  fetch.io.ins_count := io.cr.vals(1)
  val insCountTotal = fetch.io.ins_count
  val insCountCurr_q = RegInit(0.U(32.W))
  //val insCountCurr = Mux(state === sStore, insCountCurr_q + 6.U, insCountCurr_q)

  // Load inputs and weights from memory (DRAM) into scratchpads (SRAMs)
  load.io.inst <> fetch.io.inst.ld
  compute.io.inst <> fetch.io.inst.co
  store.io.inst <> fetch.io.inst.st
  //store.io.spReadCmd <> outputScratchpad.io.spReadCmd
  //store.io.spReadData <> outputScratchpad.io.spReadData
  outputScratchpad.map{_.io.spReadCmd.addr := Mux(compute.io.pSumRead.valid, compute.io.pSumRead.bits, store.io.spReadCmd.addr)}

  // TODO: FIX THIS
  store.io.spReadData.data := outputScratchpad(0).io.spReadData.data
  compute.io.pSumReadData.data := outputScratchpad(0).io.spReadData.data
  

  // Read(rd) and write(wr) from/to memory (i.e. DRAM)
  io.cr.finish := (state === sFinish)
  io.me.rd(0) <> fetch.io.me_rd
  io.me.rd(1) <> load.io.me_rd
  io.me.wr(0) <> store.io.me_wr


  val computeStartReg = RegNext((state === sLoad) && load.io.done && load.io.isFinalLoad)
  compute.io.start := computeStartReg
  switch(state){
    is(sIdle){
        when(start){
            state := sLoad
            ctr := ctr + 1.U
            insCountCurr_q := insCountCurr_q + 1.U
        }
    }
    is(sLoad){
      when(insCountCurr_q === insCountTotal){
        state := sFinish
      }.elsewhen(load.io.done){
        insCountCurr_q := insCountCurr_q + 1.U
        when(load.io.isFinalLoad){
          state := sCompute
        }.otherwise{
          state := sLoad
        }
        
        /*when(ctr === 5.U){
          state := sCompute
        }.otherwise{
          state := sLoad
          ctr := ctr + 1.U
        }*/
      }
    }
    is(sCompute){
      when(compute.io.done){
        insCountCurr_q := insCountCurr_q + 1.U
        state := sStore
        ctr := 0.U
      }
    }
    is(sStore){
      when(store.io.done){
        insCountCurr_q := insCountCurr_q + 1.U
        when(insCountCurr_q === insCountTotal){
          state := sFinish
        }.otherwise{
          state := sLoad
          ctr := 1.U
        }
      }
    }
  }
  load.io.spWrite.ready := true.B

}
