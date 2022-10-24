package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._
import gcn.core.util.MuxTree

// /** Compute.
//  *
//  * Takes instructions from fetch module. Schedules computation between PEs.
//  * Arbitrates communication betwen PE and scratchpads.
//  */
class Compute(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val io = IO(new Bundle {
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    val spReadCmd = Vec(cp.nScratchPadMem, Decoupled(new SPReadCmd))
    val spReadData = Vec(cp.nScratchPadMem, Flipped(Decoupled(new SPReadData)))
    val valid = Input(Bool())
    val done = Output(Bool())
  })
//   // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  // val req_q = Module(new Queue(new SPReadCmd, 5)) // change entry count
  // val data_q = Module(new Queue(new SPReadData, 5)) // change entry count
  val dec = Module(new ComputeDecode)
  val peArray = for (i <- 0 until 1) yield {
    Module(new PECSR)
  } 

  // PE stuff for more than 1 in number
  val peIsFree = (for (i <- 0 until cp.nPE) yield {
    (peArray(i).io.peReq.ready && !peArray(i).io.peReq.valid)
  }).reduce(_||_)
  val peFree = (for (i <- 0 until cp.nPE) yield {
    (peArray(i).io.peReq.ready && !peArray(i).io.peReq.valid).asUInt
  }).reduce(Cat(_,_))
  val peAllFree = (for (i <- 0 until cp.nPE) yield {
    peArray(i).io.peReq.ready && !peArray(i).io.peReq.valid
  }).reduce(_&&_)
  val peFreeCount = PopCount(peFree)
  // val peFreeID = PriorityEncoder(peFree)

  // state machine
  val sIdle :: sAssign :: sWait :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val ctr = RegInit(0.U(5.W))
  val done = RegInit(0.U(1.W))
  io.done := done

  val inst = RegEnable(inst_q.io.deq.bits, start)
  val saddr = Reg(UInt(M_SRAM_OFFSET_BITS.W))
  // val sdata = Reg(chiselTypeOf(data_q.io.deq.bits.data))
  val blockSizeBytes = (cp.blockSize/8)
  val spSel = RegInit(0.U(cp.nScratchPadMem))
  // val stag = Reg(chiselTypeOf(req_q.io.enq.bits.tag))


// temp
  for(i <- 0 until cp.nScratchPadMem-1){
    io.spReadCmd(i).valid := false.B
    io.spReadCmd(i).bits.tag := 0.U
    io.spReadCmd(i).bits.addr := 0.U
    io.spReadData(i).ready := false.B
  }
  val valid = RegInit(false.B)
  val currRow = RegInit(0.U(32.W))
  peArray(0).io.peReq.bits.denXSize := dec.io.xSizeDen
  peArray(0).io.peReq.bits.sramColVal := dec.io.sramColVal
  peArray(0).io.peReq.bits.sramDen := dec.io.sramDen
  peArray(0).io.peReq.bits.sramPtr := dec.io.sramPtr
  peArray(0).io.peReq.bits.rowIdx := currRow
  peArray(0).io.peReq.valid := valid
  peArray(0).io.spReadCmd.ready := (for(i <- 0 until cp.nScratchPadMem) yield {
    io.spReadCmd(i).ready && peArray(0).io.spReadCmd.bits.spSel(i)
  }).reduce(_||_)
  peArray(0).io.spData.valid := (for(i <- 0 until cp.nScratchPadMem) yield {
    io.spReadData(i).valid
  }).reduce(_||_)
  for(i <- 0 until cp.nScratchPadMem){
    io.spReadCmd(i).valid := peArray(0).io.spReadCmd.bits.spSel(i) && peArray(0).io.spReadCmd.valid
    io.spReadData(i).ready := peArray(0).io.spData.ready
    io.spReadCmd(i).bits := peArray(0).io.spReadCmd.bits.spReadCmd
    peArray(0).io.spData.bits := MuxTree(OHToUInt(peArray(0).io.spReadCmd.bits.spSel), io.spReadData.map(_.bits))
  }
  


//   // instruction queue
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)

  // control
  switch(state) {
    is(sIdle) {
      done := 0.U
      currRow := 0.U
      when(start) {
        valid := true.B
        when(peFreeCount === 1.U){
          state := sWait
        }.otherwise{
          when(currRow =/= (dec.io.ySizeSp - 1.U)){
            state := sAssign
          }.otherwise{
            state := sDone
          }
        }

      }
    }
    is(sAssign){
      currRow := currRow + 1.U
      valid := true.B
      when(peFreeCount === 1.U){
        state := sWait
      }.otherwise{
        when(currRow =/= (dec.io.ySizeSp - 1.U)){
          state := sAssign
        }.otherwise{
          state := sDone
        }
      }
    }
    is(sWait){
      valid := false.B
      when(peFreeCount =/= 0.U){
        currRow := currRow + 1.U
        valid := true.B
        when(peFreeCount === 1.U){
          state := sWait
      }.otherwise{
          state := sWait
        }
      }
    }
    is(sDone){
      when(peAllFree){
        done := 1.U
        state := sIdle
      }
    }
  }


  // // control
  // switch(state) {
  //   is(sIdle) {
  //     done := 0.U
  //     when(start) {
  //       state := sReadCmd
  //       saddr := dec.io.sramColVal
  //       stag := 0.U
  //       ctr := 1.U
  //       spSel := (1.U << 3)
  //     }
  //   }
  //   is(sReadCmd){
  //     when(req_q.io.enq.ready){
  //       state := sReadData
  //     }
  //   }
  //   is(sReadData){
  //     when(data_q.io.deq.valid){
  //       saddr := saddr + blockSizeBytes.U
  //       sdata := data_q.io.deq.bits.data
  //       spSel := (1.U << 3)
  //       when(ctr === dec.io.xSizeDen){
  //         state := sDone
  //       }.otherwise{
  //         state := sReadCmd
  //         ctr := ctr + 1.U
  //       }
  //     }
  //   }
  //   is(sDone){
  //     done := 1.U
  //     state := sIdle
  //   }
  // }
 
//   req_q.io.enq.bits.addr := saddr
//   req_q.io.enq.bits.tag := stag
//   req_q.io.enq.valid := state === sReadCmd
//   data_q.io.deq.ready := state === sReadData

//   assert((sdata(0)===0.U))

//   // instructions
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && peIsFree && io.valid

  // // Read req Queue and read data queue to 3rd scratchpad
  //   io.spReadCmd(3).bits := req_q.io.deq.bits
  //   io.spReadCmd(3).valid := req_q.io.deq.valid
  //   req_q.io.deq.ready := io.spReadCmd(3).ready

  //   data_q.io.deq.ready := (state === sReadData) 

  //   data_q.io.enq.bits := io.spReadData(3).bits
  //   data_q.io.enq.valid := io.spReadData(3).valid
  //   io.spReadData(3).ready := data_q.io.enq.ready 

}
