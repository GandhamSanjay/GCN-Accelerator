package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.collection.mutable.HashMap

case class AccParams(
    hostParams: AXIParams,
    crParams: CRParams,
    memParams: AXIParams,
    meParams: MEParams,
    coreParams: CoreParams
)

case class CRParams(
  val regBits : Int = 32,
  val nMmapReg : Int = 2 // Registers that are used by accelerator and must be initialized
) {
  val nPEEventCtr: Int = 6
  val nComputeEventCtr: Int = (CoreParams().nPE*nPEEventCtr) + 1 // 1 register for total compute time
  // // val nEventCtr: Int = nComputeEventCtr + 2 // performance counters ((D1,D2, MAC, Total_PE)*nPE + compute) + load + store  
  val nEventCtr: Int = 1
  val nSlaveReg: Int = nEventCtr + nMmapReg + 3// additional reg to launch the accelerator, indicate the end of execution and total time
  require(nMmapReg < nSlaveReg, "memory mapped registers should be atleast 1 less than slave register")
}

case class CoreParams(
  val loadInstQueueEntries: Int = 1,
  val computeInstQueueEntries: Int = 1,
  val peOutputScratchQueueEntries: Int = 10,
  val loadDataQueueEntries: Int = 10,
  val Compression: String = "CSR",
  val scratchColSize: Int = 1024*8*10,
  val scratchDenSize: Int = 1024*8*1024,
  val scratchValSize: Int = 1024*8*10,
  val scratchPtrSize: Int = 1024*8*10,
  val globalBufferSize: Int = 1024*8*1024*128,
  val nColInDense: Int = 2,
  val blockSize: Int = 32,
  val nGroups: Int = 2
) {
  val nPE: Int = nColInDense
  val PEOutputBufferDepth: Int = 32
  val aggregationBufferDepth: Int = 8
  val bankBlockSize: Int = nColInDense * blockSize
  private val ScratchPadMap: HashMap[String, Int] =
    HashMap(("CSR", 5), ("None", 2))
  var scratchSizeMap: HashMap[String, Int] = HashMap(("None", 0))
  if(Compression=="CSR"){
    scratchSizeMap = 
      HashMap(("Col", scratchColSize), ("Val", scratchValSize), ("Ptr", scratchPtrSize), ("Den", scratchDenSize), ("Out", scratchDenSize),("Psum", scratchDenSize))
  }
  val nScratchPadMem = ScratchPadMap(Compression)
}

case class MEParams
  (val nReadClients: Int = 2,
    val nWriteClients: Int = 1,
    val clientBits : Int = 3,
    val RequestQueueDepth : Int = 16,
    val meParams : Int = 18,
    val clientCmdQueueDepth : Int = 1,
    val clientTagBitWidth : Int = 21,
    val clientDataQueueDepth : Int = 16) {

  val RequestQueueMaskBits : Int = RequestQueueDepth.toInt

  require(nReadClients > 0,"nReadClients must be larger than 0")
  require(
    nWriteClients == 1,"nWriteClients must be 1, only one-write-client support")
}

case object AccKey extends Field[AccParams]

/*Shell configuration for Xilinx UltraScale+ zcu106 */
class ZcuConfig extends Config((site, here, up) => {
  case AccKey =>
    AccParams(
      hostParams = AXIParams(coherent = false,
        addrBits = 16,
        dataBits = 32,
        lenBits  = 8,
        userBits = 1),
      crParams = CRParams(),
      coreParams = CoreParams(),
      memParams = AXIParams(coherent = false,
        addrBits = 64,
        dataBits = 512,
        lenBits  = 8,
        userBits = 1),
      meParams = MEParams()
    )
})
