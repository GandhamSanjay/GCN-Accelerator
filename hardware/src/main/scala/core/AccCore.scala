/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._

/** VTAShell.
 *
 * The VTAShell is based on a VME, VCR and core. This creates a complete VTA
 * system that can be used for simulation or real hardware.
 */
class AccCore(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val host = new AXILiteClient(p(AccKey).hostParams)
  })
  val nSlaveReg = p(AccKey).coreParams.slaveReg

  // read control (AR, R)
  val sReadAddress :: sReadData :: Nil = Enum(2)
  val rstate = RegInit(sReadAddress)
  val slaveReg  = RegInit(VecInit(Seq.fill(nSlaveReg)(0.U(32.W))))
  val rdata  = WireDefault(slaveReg(0))
  val slaveRegReadSelect = (io.host.ar.bits.addr >> 2)(log2Ceil(nSlaveReg)-1,0)
  // val dataSelect = Mux(rstate === sReadData, slaveRegSelect, 0.U(log2Ceil(nSlaveReg).W))    //Do not  toggle mux select when not in valid state

  switch(rstate) {
    is(sReadAddress) {
      when(io.host.ar.valid) {
        rstate := sReadData
      }
    }
    is(sReadData) {
      when(io.host.r.ready) {
        rstate := sReadAddress
      }
    }
  }

  when(io.host.r.fire){rdata := MuxTree(slaveRegReadSelect, slaveReg)}

  io.host.ar.ready := rstate === sReadAddress
  io.host.r.valid := rstate === sReadData
  io.host.r.bits.data := rdata
  io.host.r.bits.resp := 0.U

  // Write control (AW, W, B)
  val waddr = RegInit("h_ffff".U(32.W)) // init with invalid address
  val wdata = io.host.w.bits.data
  val sWriteAddress :: sWriteData :: sWriteResponse :: Nil = Enum(3)
  val wstate = RegInit(sWriteAddress)
  val slaveRegWriteSelect = (waddr >> 2)(log2Ceil(nSlaveReg)-1,0)

  switch(wstate) {
    is(sWriteAddress) {
      when(io.host.aw.valid) {
        wstate := sWriteData
      }
    }
    is(sWriteData) {
      when(io.host.w.valid) {
        wstate := sWriteResponse
      }
    }
    is(sWriteResponse) {
      when(io.host.b.ready) {
        wstate := sWriteAddress
      }
    }
  }

  for(i <- 0 until nSlaveReg){
    when(io.host.w.fire){
      when(i.U===slaveRegWriteSelect){
        slaveReg(i) := wdata
      }
    }
  }

  when(io.host.aw.fire) { waddr := io.host.aw.bits.addr }

  io.host.aw.ready := wstate === sWriteAddress
  io.host.w.ready := wstate === sWriteData
  io.host.b.valid := wstate === sWriteResponse
  io.host.b.bits.resp := 0.U

 


}
