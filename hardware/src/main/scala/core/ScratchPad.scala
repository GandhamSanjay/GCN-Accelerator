package gcn.core

import scala.math.pow

import chisel3._
import chisel3.util._
import vta.util.config._
import os.write
import gcn.core.util.MuxTree
import ISA._


/** Scratchpad Logic.
 *
 * Load 1D and 2D tensors from main memory (DRAM) to input/weight
 * scratchpads (SRAM). Also, there is support for zero padding, while
 * doing the load. Zero-padding works on the y and x axis, and it is
 * managed by TensorPadCtrl. The TensorDataCtrl is in charge of
 * handling the way tensors are stored on the scratchpads.
 * 
 */

class SPWriteCmdWithSel(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val spCmd = new SPWriteCmd
  val spSel = UInt(cp.nScratchPadMem.W)
}

class SPWriteCmd(implicit p: Parameters) extends Bundle{
  val mp = p(AccKey).memParams
  val addr = UInt(M_SRAM_OFFSET_BITS.W)
  val data = UInt(mp.dataBits.W)
}

class SPReadCmd(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val addr = UInt(M_SRAM_OFFSET_BITS.W)
  val tag = UInt(log2Ceil(cp.nPE).W)
}

class SPReadCmdWithSel(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val spReadCmd = new SPReadCmd
  val spSel = UInt(cp.nScratchPadMem.W)
}

class SPReadData(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val mp = p(AccKey).memParams
  val data = UInt(cp.scratchBankBlockSize.W)
  val tag = UInt(log2Ceil(cp.nPE).W)
}
 
class Scratchpad(scratchType: String = "Col", debug: Boolean = false)(implicit p: Parameters)extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams

    // Scratch size params
  val blockSize = cp.blockSize
  val bankBlockSize = cp.scratchBankBlockSize
  val scratchSize = cp.scratchSizeMap(scratchType)/mp.dataBits
  val nBanks = mp.dataBits/bankBlockSize
  
  val io = IO(new Bundle {
    val spWrite = Flipped(Decoupled(new SPWriteCmd))
    val spReadCmd = Flipped(Decoupled(new SPReadCmd))
    val spReadData = Decoupled(new SPReadData)
    // val spReadCmd = Vec(nBanks, Flipped(Decoupled(new SPReadCmd)))
    // val spReadData = Vec(nBanks, Decoupled(new SPReadData))
    val out = Output(Bool())
  })
  // Scratch state
  val writeEn = WireInit(false.B)
  val readEn = WireInit(false.B)
  val sIdle :: sWrite :: sRead :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val isReadEmpty = Wire(Bool())
  isReadEmpty := true.B

  // Write req/data queue
  val write_q = Module(new Queue(new SPWriteCmd, 10))
  write_q.io.enq <> io.spWrite
  io.out := (write_q.io.count === 0.U)
  write_q.io.deq.ready := true.B
  val waddr = WireDefault(write_q.io.deq.bits.addr)
  val wdata = WireDefault(write_q.io.deq.bits.data)
  writeEn := write_q.io.deq.fire 

  // Read req/data/ queue
  val readCmd_q = Module(new Queue(new SPReadCmd, 10))
  val readData_q = Module(new Queue(new SPReadData, 10))
  readCmd_q.io.enq <> io.spReadCmd
  io.spReadData <> readData_q.io.deq
  readCmd_q.io.deq.ready := true.B
  val raddr = WireDefault(readCmd_q.io.deq.bits.addr)
  val rdata = Wire(Vec(nBanks, UInt(bankBlockSize.W)))
  val rtag = WireDefault(readCmd_q.io.deq.bits.tag)
  val bankSelPrev = RegInit(0.U(log2Ceil(nBanks).W))
  val tagPrevRead = Reg(chiselTypeOf(readData_q.io.deq.bits.tag))
  readEn := readCmd_q.io.deq.fire
  readData_q.io.enq.valid := RegNext(readEn)
  readData_q.io.enq.bits.data := MuxTree(bankSelPrev, rdata)
  readData_q.io.enq.bits.tag := tagPrevRead
  // // Banked Read req/data queue
  // val read_qSeq = Seq.fill(nBanks) {Module(new Queue(new SPReadCmd, 10))}
  // val isReadEmptyVec = for (i <- 0 until nBanks) yield {
  //   (read_qSeq(i).io.count === 0.U)
  // }
  // isReadEmpty := isReadEmptyVec.reduce(_&&_)

  // for (i <- 0 until nBanks){
  //   read_qSeq(i).io.enq <> io.spReadCmd(i)
  //   val raddr = WireDefault(read_qSeq(i).io.deq.bits.addr)
  // }


  val ram = Seq.fill(nBanks){
    SyncReadMem(scratchSize, UInt(bankBlockSize.W))
  }

  when(writeEn){
    val writeIdx = waddr >> log2Ceil(blockSize/8)
    for (i <- 0 until (nBanks)){
      ram(i).write(writeIdx, wdata((i+1)*bankBlockSize - 1, i*bankBlockSize))
    }
  }
 
  val raddrByteAlign =  (raddr >> log2Ceil(bankBlockSize/8))
  val bankSel = (raddrByteAlign)(log2Ceil(nBanks) - 1, 0)
  val bankIdx = (raddrByteAlign) >> (log2Ceil(nBanks))
  bankSelPrev := bankSel
  tagPrevRead := rtag
  for (i <- 0 until (nBanks)){
    rdata(i) := ram(i).read(bankIdx, (bankSel === i.U) && readEn )
  // }.otherwise{
  //   for (i <- 0 until (nBanks)){
  //     rdata(i) := 0.U
  //   }
  }
  



}

