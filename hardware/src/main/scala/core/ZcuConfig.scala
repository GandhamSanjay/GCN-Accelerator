package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._

case class AccParams(
    hostParams: AXIParams,
    crParams: CRParams,
    memParams: AXIParams,
    meParams: MEParams,
    coreParams: CoreParams
)

case class CRParams(
  val nSlaveReg: Int = 6,
  val nEventCtr: Int = 0,
  val regBits : Int = 32,
  val nMmapReg : Int = 2
) {
  require(nSlaveReg > 0, "Core slave register must be atleast 1")
  require(nMmapReg < nSlaveReg, "memory mapped registers should be atleast 1 less than slave register")
}

case class CoreParams(
  val loadInstQueueEntries: Int = 1
) {
  require(loadInstQueueEntries > 0, "instQueueEntries must be atleast 1")
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
        addrBits = 32,
        dataBits = 512,
        lenBits  = 1,
        userBits = 1),
      meParams = MEParams()
    )
})
