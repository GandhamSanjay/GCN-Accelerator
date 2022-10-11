package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._

case class AccParams(
    hostParams: AXIParams,
    coreParams: CoreParams
)

case class CoreParams(
  val slaveReg: Int = 4
) {
  require(slaveReg > 0, "Core slave register must be atleast 1")
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
      coreParams = CoreParams(
        slaveReg = 4
      )
    )
})
