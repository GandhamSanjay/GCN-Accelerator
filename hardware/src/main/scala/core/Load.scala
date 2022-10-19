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

/** Load.
 *
 * Load inputs and weights from memory (DRAM) into scratchpads (SRAMs).
 * This module instantiate the TensorLoad unit which is in charge of
 * loading 1D and 2D tensors to scratchpads, so it can be used by
 * other modules such as Compute.
 */
class Load(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val io = IO(new Bundle {
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    val me_rd = new MEReadMaster
  })
  // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), p(AccKey).coreParams.loadInstQueueEntries))
  val dec = Module(new LoadDecode)
  // state machine
  val sIdle :: sStride :: sSeq :: sSeqCmd :: sSeqReadData ::Nil = Enum(5)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val done = true.B
  val inst = RegEnable(inst_q.io.deq.bits, start)
  val nBlockPerTransfer = mp.dataBits / 32
  val transferTotal = WireDefault((dec.io.xSize)-1.U >> log2Ceil(nBlockPerTransfer)) + 1.U
  val transferRem = Reg(chiselTypeOf(dec.io.xSize))
  val maxTransferPerReq = (1 << mp.lenBits).U
  val raddr = Reg(chiselTypeOf(io.me_rd.cmd.bits.addr))
  val rlen = Reg(chiselTypeOf(io.me_rd.cmd.bits.len))
  val rlenRem = Reg(chiselTypeOf(io.me_rd.cmd.bits.len))
  val transferMaxSizeBytes = (mp.lenBits + 1) << log2Ceil(mp.dataBits / 8)
  val rdata = Reg(chiselTypeOf(io.me_rd.data.bits.data))

  // instruction queue
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)

  // control
  switch(state) {
    is(sIdle) {
      when(start) {
        when(dec.io.isSeq){
          state := sSeqCmd
          raddr := dec.io.sramOffset
          when(transferTotal < maxTransferPerReq){
            rlen := transferTotal - 1.U
            rlenRem := transferTotal - 1.U
            transferRem := 0.U
          }.otherwise{
            rlen := maxTransferPerReq - 1.U
            rlenRem := maxTransferPerReq - 1.U
            transferRem := transferTotal - (maxTransferPerReq)
          }
        }.otherwise{
          state := sStride
        }
      }
    }
    is(sSeqCmd){
      when(io.me_rd.cmd.ready){
        state := sSeqReadData 
       }
    }
    is(sSeqReadData){
      when(io.me_rd.data.valid){
        rdata := io.me_rd.data.bits.data
          when(rlenRem === 0.U){
            when(transferRem === 0.U){
              state := sIdle
            }.otherwise{
              state := sSeqCmd
              raddr := raddr + transferMaxSizeBytes.U
              when(transferRem < maxTransferPerReq){
                rlen := transferRem - 1.U
                rlenRem := transferRem - 1.U
                transferRem := 0.U
              }.otherwise{
                rlen := maxTransferPerReq - 1.U
                rlenRem := maxTransferPerReq - 1.U
                transferRem := transferRem - maxTransferPerReq
              }
            }
          }.otherwise{
            rlenRem := rlenRem - rlen
          }
      }
    }
    is(sStride) {
      when(done){
        state := sIdle
      }
    }
  }


  // instructions
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle)
  
  // dram read
  io.me_rd.cmd.bits.len := rlen
  io.me_rd.cmd.bits.tag := dec.io.sramOffset
  io.me_rd.cmd.bits.addr := raddr
  io.me_rd.cmd.valid := (state === sSeqCmd)
  io.me_rd.data.ready := true.B

}
