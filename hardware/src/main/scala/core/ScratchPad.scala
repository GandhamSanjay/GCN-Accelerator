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

class SPWriteCmd(val scratchType: String = "Col")(implicit p: Parameters) extends Bundle{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val addr = UInt(M_SRAM_OFFSET_BITS.W)
  val data = if(scratchType == "Out"){UInt(cp.blockSize.W)}else{UInt(mp.dataBits.W)}
}

class SPReadCmd(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val addr = UInt(M_SRAM_OFFSET_BITS.W)
}

class SPReadCmdWithSel(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val spReadCmd = new SPReadCmd
  val spSel = UInt(cp.nScratchPadMem.W)
}

class SPReadData(val scratchType: String = "Col")(implicit p: Parameters) extends Bundle{
  val cp = p(AccKey).coreParams
  val mp = p(AccKey).memParams
  val data = if(scratchType == "Out"){UInt(mp.dataBits.W)}else{UInt(cp.blockSize.W)}
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
    val spWrite = Input(new SPWriteCmd)
    val spReadCmd = Input(new SPReadCmd)
    val spReadData = Output(new SPReadData)
    val writeEn = Input(Bool())
  })

  // Write
  val rdataSliced = Wire(UInt(cp.blockSize.W))
  val waddr = WireDefault(io.spWrite.addr)
  val wdata = WireDefault(io.spWrite.data)

  // Read

  val raddr = WireDefault(io.spReadCmd.addr)
  val rdata = Wire(Vec(nBanks, UInt(bankBlockSize.W)))
  val bankSelPrev = RegInit(0.U(log2Ceil(nBanks).W))

  val ram = Seq.fill(nBanks){
    SyncReadMem(scratchSize, UInt(bankBlockSize.W))
  }

  when(io.writeEn){
    val writeIdx = waddr >> log2Ceil(blockSize/8)
    for (i <- 0 until (nBanks)){
      ram(i).write(writeIdx, wdata((i+1)*bankBlockSize - 1, i*bankBlockSize))
    }
  }
 
  val raddrByteAlign =  (raddr >> log2Ceil(bankBlockSize/8))
  val bankSel = (raddrByteAlign)(log2Ceil(nBanks) - 1, 0)
  val bankIdx = (raddrByteAlign) >> (log2Ceil(nBanks))
  val rdataSel = (RegNext(raddr) >> log2Ceil(blockSize/8))(log2Ceil(bankBlockSize/blockSize)-1,0)
  rdataSliced := (MuxTree(bankSelPrev, rdata) >> (rdataSel << log2Ceil(blockSize)))(cp.blockSize - 1, 0)
  bankSelPrev := bankSel
  for (i <- 0 until (nBanks)){
    rdata(i) := ram(i).read(bankIdx, (bankSel === i.U))
  }
  io.spReadData.data := rdataSliced
}

class OutputScratchpad(scratchType: String = "Out", debug: Boolean = false)(implicit p: Parameters)extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams

    // Scratch size params
  val blockSize = cp.blockSize
  val scratchSize = cp.scratchSizeMap(scratchType)/mp.dataBits
  val nBanks = mp.dataBits/blockSize
  
  val io = IO(new Bundle {
    val spWrite = Input(new SPWriteCmd(scratchType = "Out"))
    val spReadCmd = Input(new SPReadCmd)
    val spReadData = Output(new SPReadData(scratchType = "Out"))
    val writeEn = Input(Bool())
  })

  // Write
  val waddr = WireDefault(io.spWrite.addr)
  val waddrByteAlign =  (waddr >> log2Ceil(blockSize/8))
  val wdata = WireDefault(io.spWrite.data)
  val writeBankSel = (waddrByteAlign)(log2Ceil(nBanks) - 1, 0)
  val writeIdx = waddrByteAlign >> log2Ceil(nBanks)
  // Read

  val raddr = WireDefault(io.spReadCmd.addr)
  val rdata = Wire(Vec(nBanks, UInt(blockSize.W)))

  val ram = Seq.fill(nBanks){
    SyncReadMem(scratchSize, UInt(blockSize.W))
  }


  for (i <- 0 until (nBanks)){
    when(io.writeEn && (writeBankSel === i.U)){
      ram(i).write(writeIdx, wdata)
    }
  }
 
  val readIdx = raddr >> log2Ceil(nBanks*blockSize/8)
  for (i <- 0 until (nBanks)){
    rdata(i) := ram(i).read(readIdx, true.B)
  }
  io.spReadData.data := rdata.reverse.reduce(Cat(_,_))

  assert(ram(0)(1)=/=19.U)
}
