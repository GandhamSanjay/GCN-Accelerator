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
    val spWrite = Vec(cp.nScratchPadMem, Flipped(Decoupled(new SPWriteCmd)))
    val valid = Input(Bool())
    val done = Output(Bool())
  })
//   // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  val dec = Module(new ComputeDecode)
  val peArray = for (i <- 0 until cp.nPE) yield {
    Module(new PECSR)
  } 


  val peAllFree = if(cp.nPE>1){(for (i <- 0 until cp.nPE) yield {
    peArray(i).io.peReq.ready
  }).reduce(_&&_)}else{ peArray(0).io.peReq.ready}

  // state machine
  val sIdle :: sWait :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val ctr = RegInit(0.U(5.W))

  val inst = RegEnable(inst_q.io.deq.bits, start)
  val blockSizeBytes = (cp.blockSize/8)
  val currRow = RegInit(0.U(32.W))

  for(i <- 0 until cp.nPE){
    peArray(i).io.peNum := i.U
    peArray(i).io.peReq.valid := start
    peArray(i).io.peReq.bits.spYSize := dec.io.ySizeSp
  }


  assert (dec.io.ySizeSp =/= 9.U)


// Assigns the lowest row index to the first available PE , 2nd lowest row index to next 
// available PE and so on during the same clock cycle. Sets the valid high according to
// number of higher priority PE that are free and number of rows remaining to be assigned

  for(i <- 0 until cp.nPE){
    peArray(i).io.peReq.bits.denXSize := dec.io.xSizeDen
    peArray(i).io.peReq.bits.sramColVal := dec.io.sramColVal
    peArray(i).io.peReq.bits.sramDen := dec.io.sramDen
    peArray(i).io.peReq.bits.sramPtr := dec.io.sramPtr
    peArray(i).io.spWrite <> io.spWrite
  }
  val done = RegInit(0.U(32.W))
  io.done := done

  // instruction queue
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)

  // control
  switch(state) {
    is(sIdle) {
      done := false.B
      currRow := 0.U
      when(start){
        state := sWait
      }
    }
    is(sWait){
      when(peAllFree){
        done := true.B
        state := sIdle
      }
    }
  }

  // instructions
  inst_q.io.enq <> io.inst
  inst_q.io.deq.ready := (state === sIdle) && io.valid
}
