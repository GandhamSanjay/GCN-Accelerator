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
import vta.util.genericbundle._
import gcn.core.util._

/** CRBase. Parametrize base class. */
abstract class CRBase(implicit p: Parameters) extends GenericParameterizedBundle(p)

/** CRMaster.
 *
 * This is the master interface used by CR in the Accelerator to control
 * the Core unit.
 */
class CRMaster(implicit p: Parameters) extends CRBase {
  val launch = Output(Bool())
  val finish = Input(Bool())
}

/** CRSlave.
 *
 * This is the slave interface used by the Core module to communicate
 * to the CR in the Accelerator.
 */
class CRClient(implicit p: Parameters) extends CRBase {
  val launch = Input(Bool())
  val finish = Output(Bool())
}
/** Control Registers (CR).
 *
 * This unit provides control registers (32 bits) to be used by a control
 * unit. These registers are read-only by the core.
 * ****************** TO-DO *************************
 * Add event counter registers to CR to
 */
class CR(implicit p: Parameters) extends Module {
  val coreParams = p(AccKey).coreParams
  val io = IO(new Bundle {
    val host = new AXILiteClient(p(AccKey).hostParams)
    val cr = new CRMaster
    val eventCounters = if (p(AccKey).coreParams.eventCtr > 0)  Some(Output(RegInit(VecInit(Seq.fill(coreParams.eventCtr)(0.U(32.W)))))) else None
  })
  val nSlaveReg = p(AccKey).coreParams.slaveReg

  // read control (AR, R)
  val sReadAddress :: sReadData :: Nil = Enum(2)
  val rstate = RegInit(sReadAddress)
  val slaveReg  = RegInit(VecInit(Seq.fill(nSlaveReg)(0.U(32.W))))
  val rdata  = WireDefault(slaveReg(0))
  val slaveRegReadSelect = (io.host.ar.bits.addr >> 2)(log2Ceil(nSlaveReg)-1,0)
  // val dataSelect = Mux(rstate === sReadData, slaveRegSelect, 0.U(log2Ceil(nSlaveReg).W))    //Do not  toggle mux select when not in valid state
  val pulse = Wire(UInt(32.W))
  pulse := slaveReg(0)(0) && !RegNext(slaveReg(0)(0))
  
  // CR Master IO 
  io.cr.launch := pulse

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
