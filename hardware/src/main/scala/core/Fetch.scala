
package gcn.core

import scala.math.pow
import scala.math.sqrt
import chisel3._
import chisel3.util._
import vta.util.config._
import vta.util._

/** Fetch.
 *
 * The fetch unit reads instructions (tasks) from memory (i.e. DRAM), using the
 * Memory Engine (ME), and push them into an instruction queue called
 * inst_q. Once the instruction queue is full, instructions are dispatched to
 * the Load, Compute and Store module queues based on the instruction opcode.
 * After draining the queue, the fetch unit checks if there are more instructions
 * via the ins_count register which is written by the host.
 *
 * Additionally, instructions are read into two chunks (see sReadLSB and sReadMSB)
 * because we are using a DRAM payload of 8-bytes or half of a VTA instruction.
 * This should be configurable for larger payloads, i.e. 64-bytes, which can load
 * more than one instruction at the time. Finally, the instruction queue is
 * sized (entries_q), depending on the maximum burst allowed in the memory.
 */
class Fetch(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val vp = p(AccKey).crParams
  val mp = p(AccKey).memParams
  val io = IO(new Bundle {
    val launch = Input(Bool())
    val ins_baddr = Input(UInt(mp.addrBits.W))
    val ins_count = Input(UInt(vp.regBits.W))
    val me_rd = new MEReadMaster
    val inst = new Bundle {
      val ld = Decoupled(UInt(INST_BITS.W))
      val co = Decoupled(UInt(INST_BITS.W))
      // val st = Decoupled(UInt(INST_BITS.W))
    }
  })
//   val entries_q = 1 << (mp.lenBits - 1) // one-instr-every-two-me-word
  val insPerTransfer = (mp.dataBits/INST_BITS)
  val entries_q = (1 << mp.lenBits)
  val inst_q = Module(new SyncQueue(UInt(mp.dataBits.W), entries_q))
  val dec = Module (new FetchDecode)

  val s1_launch = RegNext(io.launch, init = false.B)
  val pulse = io.launch & ~s1_launch

  val raddr = Reg(chiselTypeOf(io.me_rd.cmd.bits.addr))
  val rlen = Reg(chiselTypeOf(io.me_rd.cmd.bits.len))
  val ilen = Reg(chiselTypeOf(io.me_rd.cmd.bits.len))

  val xrem = Reg(chiselTypeOf(io.ins_count))
  val xsize = (io.ins_count >> log2Ceil(insPerTransfer)) - 1.U
  val xmax = (1 << mp.lenBits).U
  val xmax_bytes = ((1 << mp.lenBits) * mp.dataBits / 8).U

  val sIdle :: sReadCmd :: sRead :: sDrain :: sSplit :: Nil = Enum(5)
  val state = RegInit(sIdle)
  val packInst = Reg(chiselTypeOf(io.me_rd.data.bits.data))
  val packInstSelect = RegInit(0.U(log2Ceil(mp.dataBits).W))
  val deqReady = Wire(Bool())
  val inst = RegInit(0.U(mp.dataBits.W))

  // control
  switch(state) {
    is(sIdle) {
      when(pulse) {
        state := sReadCmd
        when(xsize < xmax) {
          rlen := xsize
          ilen := xsize
          xrem := 0.U
        }.otherwise {
          rlen := xmax - 1.U
          ilen := xmax - 1.U
          xrem := xsize - xmax
        }
      }
    }
    is(sReadCmd) {
      when(io.me_rd.cmd.ready) {
        state := sRead
      }
    }
    is(sRead) {
      when(io.me_rd.data.valid) {
        when(inst_q.io.count === ilen) {
          state := sDrain
          packInstSelect := 0.U
        }.otherwise {
          state := sRead
        }
      }
    }
    is(sDrain) {
      when(inst_q.io.count === 0.U) {
        when(xrem === 0.U) {
          state := sIdle
        }.elsewhen(xrem < xmax) {
          state := sReadCmd
          rlen := xrem
          ilen := xrem
          xrem := 0.U
        }.otherwise {
          state := sReadCmd
          rlen := xmax - 1.U
          ilen := xmax - 1.U
          xrem := xrem - xmax
        }
      }.otherwise{
        state := sSplit
      }
    }
    is(sSplit){
      when(io.inst.ld.fire || io.inst.co.fire ){
        when(packInstSelect === (mp.dataBits - INST_BITS).U){
          packInstSelect := 0.U
          state := sDrain
        }.otherwise{
          packInstSelect := packInstSelect + INST_BITS.U
        }
      }
    }
  }

  // read instructions from dram
  when(state === sIdle) {
    raddr := io.ins_baddr
  }.elsewhen(state === sDrain && inst_q.io.count === 0.U && xrem =/= 0.U) {
    raddr := raddr + xmax_bytes
  }

  io.me_rd.cmd.valid := state === sReadCmd
  io.me_rd.cmd.bits.addr := raddr
  io.me_rd.cmd.bits.len := rlen
  io.me_rd.cmd.bits.tag := 0.U // Cannot reorder requests as a queue is used

  io.me_rd.data.ready := (state === sRead) && inst_q.io.enq.ready


  inst_q.io.enq.valid := io.me_rd.data.valid
  inst_q.io.enq.bits := io.me_rd.data.bits.data


  // instruction queues
  io.inst.ld.valid := dec.io.isLoad & io.inst.ld.ready & state === sSplit
  io.inst.co.valid := dec.io.isCompute & io.inst.co.ready & state === sSplit
  // io.inst.st.valid := dec.io.isStore & io.inst.st.ready & state === sSplit

  assert(!(inst_q.io.deq.valid & state === sDrain) || dec.io.isLoad || dec.io.isCompute || dec.io.isStore,
    "-F- Fetch: Unknown instruction type")

  io.inst.ld.bits := (inst >> (packInstSelect))(INST_BITS - 1, 0)
  io.inst.co.bits := (inst >> (packInstSelect))(INST_BITS - 1, 0)
  // io.inst.st.bits := (inst >> (packInstSelect))(INST_BITS - 1, 0)

  // check if selected queue is ready
  val deq_sel = Cat(dec.io.isCompute, dec.io.isStore, dec.io.isLoad).asUInt
  val deq_ready =
    MuxLookup(deq_sel,
      false.B, // default
      Array(
        "h_01".U -> io.inst.ld.ready,
        // "h_02".U -> io.inst.st.ready,
        "h_04".U -> io.inst.co.ready
      ))

  // dequeue instruction
  inst_q.io.deq.ready := deq_ready & inst_q.io.deq.valid & state === sDrain

  deqReady := (state === sDrain)
  when(inst_q.io.deq.fire){ inst := inst_q.io.deq.bits}
  inst_q.io.deq.ready := deqReady & inst_q.io.deq.valid & state === sDrain


  // decode
  dec.io.inst := (inst >> (packInstSelect))(INST_BITS - 1, 0)

  // debug
  if (debug) {
    when(state === sIdle && pulse) {
      printf("[Fetch] Launch\n")
    }
    // instruction
    when(inst_q.io.deq.fire) {
      when(dec.io.isLoad) {
        printf("[Fetch] [instruction decode] [L] %x\n", inst_q.io.deq.bits)
      }
      when(dec.io.isCompute) {
        printf("[Fetch] [instruction decode] [C] %x\n", inst_q.io.deq.bits)
      }
      when(dec.io.isStore) {
        printf("[Fetch] [instruction decode] [S] %x\n", inst_q.io.deq.bits)
      }
    }
  }
}
