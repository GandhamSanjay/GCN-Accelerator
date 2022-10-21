package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._

// /** Compute.
//  *
//  * Takes instructions from fetch module. Schedules computation between PEs.
//  * Arbitrates communication betwen PE and scratchpads.
//  */
class Compute(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val io = IO(new Bundle {
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    val spReadCmd = Vec(cp.nScratchPadMem, Decoupled(new SPReadCmd))
    val spReadData = Vec(cp.nScratchPadMem, Flipped(Decoupled(new SPReadData)))
    val valid = Input(Bool())
    val done = Output(Bool())
  })
//   // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  val req_q = Module(new Queue(new SPReadCmd, 5)) // change entry count
  val data_q = Module(new Queue(new SPReadData, 5)) // change entry count
  val dec = Module(new ComputeDecode)
//   // state machine
  val sIdle :: sReadCmd :: sReadData :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val ctr = RegInit(0.U(5.W))
  val done = RegInit(0.U(1.W))
  io.done := done

  val inst = RegEnable(inst_q.io.deq.bits, start)
  val saddr = Reg(UInt(M_SRAM_OFFSET_BITS.W))
  val sdata = Reg(chiselTypeOf(data_q.io.deq.bits.data))
  val blockSizeBytes = (cp.blockSize/8)
  val spSel = RegInit(0.U(cp.nScratchPadMem))
  val stag = Reg(chiselTypeOf(req_q.io.enq.bits.tag))


// tie-off
  for(i <- 0 until cp.nScratchPadMem-1){
    io.spReadCmd(i).valid := false.B
    io.spReadCmd(i).bits.tag := 0.U
    io.spReadCmd(i).bits.addr := 0.U
    io.spReadData(i).ready := false.B
  }


//   // instruction queue
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)

  // control
  switch(state) {
    is(sIdle) {
      done := 0.U
      when(start) {
        state := sReadCmd
        saddr := dec.io.sramColVal
        stag := 0.U
        ctr := 1.U
        spSel := (1.U << 3)
      }
    }
    is(sReadCmd){
      when(req_q.io.enq.ready){
        state := sReadData
      }
    }
    is(sReadData){
      when(data_q.io.deq.valid){
        saddr := saddr + blockSizeBytes.U
        sdata := data_q.io.deq.bits.data
        spSel := (1.U << 3)
        when(ctr === dec.io.xSizeDen){
          state := sDone
        }.otherwise{
          state := sReadCmd
          ctr := ctr + 1.U
        }
      }
    }
    is(sDone){
      done := 1.U
      state := sIdle
    }
  }

  req_q.io.enq.bits.addr := saddr
  req_q.io.enq.bits.tag := stag
  req_q.io.enq.valid := state === sReadCmd
  data_q.io.deq.ready := state === sReadData

  assert((sdata(0)===0.U))

//   // instructions
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && io.valid

  // Read req Queue and read data queue to 3rd scratchpad
    io.spReadCmd(3).bits := req_q.io.deq.bits
    io.spReadCmd(3).valid := req_q.io.deq.valid
    req_q.io.deq.ready := io.spReadCmd(3).ready

    data_q.io.deq.ready := (state === sReadData) 

    data_q.io.enq.bits := io.spReadData(3).bits
    data_q.io.enq.valid := io.spReadData(3).valid
    io.spReadData(3).ready := data_q.io.enq.ready 

}
