package gcn.core

import chisel3._
import chisel3.stage._
import chisel3.util._
import vta.util.config._

class Wrapper(implicit p: Parameters) extends RawModule{

  val ap_clk = IO(Input(Clock()))
  val ap_rst_n = IO(Input(Bool()))
  val hp = p(AccKey).hostParams
  val s_axi_control = IO(new XilinxAXILiteClient(hp))
  val finish = IO(Output(Bool()))
  val m_axi_gmem = IO(new XilinxAXIMaster(p(AccKey).memParams))

  val cr = withClockAndReset(clock = ap_clk, reset = ~ap_rst_n) {
    Module(new CR)
  }
  val core = withClockAndReset(clock = ap_clk, reset = ~ap_rst_n) {
    Module(new Core)
  }
  val me = withClockAndReset(clock = ap_clk, reset = ~ap_rst_n) {
    Module(new ME)
  }
  core.io.cr <> cr.io.cr
  me.io.me <> core.io.me
  finish := cr.io.cr.finish


   // memory
  m_axi_gmem.AWVALID := me.io.mem.aw.valid
  me.io.mem.aw.ready := m_axi_gmem.AWREADY
  m_axi_gmem.AWADDR := me.io.mem.aw.bits.addr
  m_axi_gmem.AWID := me.io.mem.aw.bits.id
  m_axi_gmem.AWUSER := me.io.mem.aw.bits.user
  m_axi_gmem.AWLEN := me.io.mem.aw.bits.len
  m_axi_gmem.AWSIZE := me.io.mem.aw.bits.size
  m_axi_gmem.AWBURST := me.io.mem.aw.bits.burst
  m_axi_gmem.AWLOCK := me.io.mem.aw.bits.lock
  m_axi_gmem.AWCACHE := me.io.mem.aw.bits.cache
  m_axi_gmem.AWPROT := me.io.mem.aw.bits.prot
  m_axi_gmem.AWQOS := me.io.mem.aw.bits.qos
  m_axi_gmem.AWREGION := me.io.mem.aw.bits.region

  m_axi_gmem.WVALID := me.io.mem.w.valid
  me.io.mem.w.ready := m_axi_gmem.WREADY
  m_axi_gmem.WDATA := me.io.mem.w.bits.data
  m_axi_gmem.WSTRB := me.io.mem.w.bits.strb
  m_axi_gmem.WLAST := me.io.mem.w.bits.last
  m_axi_gmem.WID := me.io.mem.w.bits.id
  m_axi_gmem.WUSER := me.io.mem.w.bits.user

  me.io.mem.b.valid := m_axi_gmem.BVALID
  m_axi_gmem.BREADY := me.io.mem.b.valid
  me.io.mem.b.bits.resp := m_axi_gmem.BRESP
  me.io.mem.b.bits.id := m_axi_gmem.BID
  me.io.mem.b.bits.user := m_axi_gmem.BUSER

  m_axi_gmem.ARVALID := me.io.mem.ar.valid
  me.io.mem.ar.ready := m_axi_gmem.ARREADY
  m_axi_gmem.ARADDR := me.io.mem.ar.bits.addr
  m_axi_gmem.ARID := me.io.mem.ar.bits.id
  m_axi_gmem.ARUSER := me.io.mem.ar.bits.user
  m_axi_gmem.ARLEN := me.io.mem.ar.bits.len
  m_axi_gmem.ARSIZE := me.io.mem.ar.bits.size
  m_axi_gmem.ARBURST := me.io.mem.ar.bits.burst
  m_axi_gmem.ARLOCK := me.io.mem.ar.bits.lock
  m_axi_gmem.ARCACHE := me.io.mem.ar.bits.cache
  m_axi_gmem.ARPROT := me.io.mem.ar.bits.prot
  m_axi_gmem.ARQOS := me.io.mem.ar.bits.qos
  m_axi_gmem.ARREGION := me.io.mem.ar.bits.region

  me.io.mem.r.valid := m_axi_gmem.RVALID
  m_axi_gmem.RREADY := me.io.mem.r.ready
  me.io.mem.r.bits.data := m_axi_gmem.RDATA
  me.io.mem.r.bits.resp := m_axi_gmem.RRESP
  me.io.mem.r.bits.last := m_axi_gmem.RLAST
  me.io.mem.r.bits.id := m_axi_gmem.RID
  me.io.mem.r.bits.user := m_axi_gmem.RUSER

  // host
  cr.io.host.aw.valid := s_axi_control.AWVALID
  s_axi_control.AWREADY := cr.io.host.aw.ready
  cr.io.host.aw.bits.addr := s_axi_control.AWADDR

  cr.io.host.w.valid := s_axi_control.WVALID
  s_axi_control.WREADY := cr.io.host.w.ready
  cr.io.host.w.bits.data := s_axi_control.WDATA
  cr.io.host.w.bits.strb := s_axi_control.WSTRB

  s_axi_control.BVALID := cr.io.host.b.valid
  cr.io.host.b.ready := s_axi_control.BREADY
  s_axi_control.BRESP := cr.io.host.b.bits.resp

  cr.io.host.ar.valid := s_axi_control.ARVALID
  s_axi_control.ARREADY := cr.io.host.ar.ready
  cr.io.host.ar.bits.addr := s_axi_control.ARADDR

  s_axi_control.RVALID := cr.io.host.r.valid
  cr.io.host.r.ready := s_axi_control.RREADY
  s_axi_control.RDATA := cr.io.host.r.bits.data
  s_axi_control.RRESP := cr.io.host.r.bits.resp



}

// Executable object generate verilog
object DefaultTemplate extends App {
  implicit val p: Parameters = new ZcuConfig
  val chiselStage = new chisel3.stage.ChiselStage
  chiselStage.execute(
    Array(
      "-e", "mverilog", 
      "--target-dir", "verilog"), 
    Seq(ChiselGeneratorAnnotation(() => new Wrapper()))
  )
}