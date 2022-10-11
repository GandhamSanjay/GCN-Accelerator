package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._

class Wrapper(implicit p: Parameters) extends RawModule{

  val ap_clk = IO(Input(Clock()))
  val ap_rst_n = IO(Input(Bool()))
  val hp = p(AccKey).hostParams
  val s_axi_control = IO(new XilinxAXILiteClient(hp))

  val core = withClockAndReset(clock = ap_clk, reset = ~ap_rst_n) {
    Module(new AccCore)
  }

  // host
  core.io.host.aw.valid := s_axi_control.AWVALID
  s_axi_control.AWREADY := core.io.host.aw.ready
  core.io.host.aw.bits.addr := s_axi_control.AWADDR

  core.io.host.w.valid := s_axi_control.WVALID
  s_axi_control.WREADY := core.io.host.w.ready
  core.io.host.w.bits.data := s_axi_control.WDATA
  core.io.host.w.bits.strb := s_axi_control.WSTRB

  s_axi_control.BVALID := core.io.host.b.valid
  core.io.host.b.ready := s_axi_control.BREADY
  s_axi_control.BRESP := core.io.host.b.bits.resp

  core.io.host.ar.valid := s_axi_control.ARVALID
  s_axi_control.ARREADY := core.io.host.ar.ready
  core.io.host.ar.bits.addr := s_axi_control.ARADDR

  s_axi_control.RVALID := core.io.host.r.valid
  core.io.host.r.ready := s_axi_control.RREADY
  s_axi_control.RDATA := core.io.host.r.bits.data
  s_axi_control.RRESP := core.io.host.r.bits.resp



}

object DefaultTemplate extends App {
  implicit val p: Parameters = new ZcuConfig
  (new chisel3.stage.ChiselStage).emitVerilog(new Wrapper, args)
}