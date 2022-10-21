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
//   val data_qEntries = (1 << mp.lenBits)
//   val data_q = Module(new Queue(new SPWriteCmdWithSel, data_qEntries))
//   val dec = Module(new LoadDecode)
//   // state machine
  val sIdle :: sBusy :: sDone ::Nil = Enum(3)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val delayCtr = RegInit(0.U(5.W))
  val done = RegInit(0.U(1.W))
  io.done := done
//   val done = true.B
//   val inst = RegEnable(inst_q.io.deq.bits, start)
//   val nBlockPerTransfer = mp.dataBits / 32
//   val transferTotal = WireDefault((dec.io.xSize)-1.U >> log2Ceil(nBlockPerTransfer)) + 1.U
//   val transferRem = Reg(chiselTypeOf(dec.io.xSize))
//   val maxTransferPerReq = (1 << mp.lenBits).U
//   val raddr = Reg(chiselTypeOf(io.me_rd.cmd.bits.addr))
//   val rlen = Reg(chiselTypeOf(io.me_rd.cmd.bits.len))
//   val rlenRem = Reg(chiselTypeOf(io.me_rd.cmd.bits.len))
//   val transferMaxSizeBytes = (mp.lenBits + 1) << log2Ceil(mp.dataBits / 8)
//   val saddr = Reg(UInt(M_SRAM_OFFSET_BITS.W))
//   val mask = UInt((mp.dataBits/32).W)

// tie-off
  for(i <- 0 until cp.nScratchPadMem){
    io.spReadCmd(i).valid := false.B
    io.spReadCmd(i).bits.tag := 0.U
    io.spReadCmd(i).bits.addr := 0.U
    io.spReadData(i).ready := false.B
  }


//   // instruction queue
//   dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)

//   val scratchSel = Cat(dec.io.isCol, dec.io.isPtr, !dec.io.isSeq, dec.io.isVal) // col,ptr,den,val

  // control
  switch(state) {
    is(sIdle) {
      done := 0.U
      when(start) {
        state := sBusy
        delayCtr := 1.U
      }
    }
    is(sBusy){
      when(delayCtr === 0.U){
        state := sDone
      }.otherwise{
        delayCtr := delayCtr + 1.U
      }
    }
    is(sDone){
      done := 1.U
      state := sIdle
    }
  }


//   // instructions
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && io.valid

//   // data queue
//   data_q.io.enq.bits.spCmd.data := io.me_rd.data.bits.data
//   data_q.io.enq.bits.spSel := scratchSel
//   data_q.io.enq.bits.spCmd.addr := saddr + (mp.dataBits/8).U
//   data_q.io.enq.valid := (state === sSeqReadData) && io.me_rd.data.valid
  
//   // dram read
//   io.me_rd.cmd.bits.len := rlen
//   io.me_rd.cmd.bits.tag := dec.io.sramOffset
//   io.me_rd.cmd.bits.addr := raddr
//   io.me_rd.cmd.valid := (state === sSeqCmd) && (data_q.io.count === 0.U)
//   io.me_rd.data.ready := true.B

//   // Data Write Queue to multiple scratchpad
//   for(i <- 0 until cp.nScratchPadMem){
//     io.spWrite(i).bits := data_q.io.deq.bits.spCmd
//     io.spWrite(i).valid := data_q.io.deq.bits.spSel(i) && data_q.io.deq.valid
//     data_q.io.deq.ready := data_q.io.deq.bits.spSel(i) && io.spWrite(i).ready 
//   }

}
