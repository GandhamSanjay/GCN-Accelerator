package gcn.core

import chisel3._
import chisel3.util._
import vta.util.config._
import scala.math._
import gcn.core.util._
// /** Compute.
//  *
//  * Takes instructions from fetch module. Schedules computation between PEs.
//  * Arbitrates communication betwen PE and scratchpads.
//  */
class Compute(debug: Boolean = false)(implicit p: Parameters) extends Module with ISAConstants{
  val mp = p(AccKey).memParams
  val cp = p(AccKey).coreParams
  val regBits = p(AccKey).crParams.regBits
  val io = IO(new Bundle {
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    val spWrite = Vec(cp.nScratchPadMem, Flipped(Decoupled(new SPWriteCmd)))
    val valid = Input(Bool())
    val done = Output(Bool())
    val spOutWrite = Decoupled(new SPWriteCmd(scratchType = "Col"))
    val ecnt = Vec(p(AccKey).crParams.nComputeEventCtr, ValidIO(UInt(regBits.W)))
  })

  // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  val dec = Module(new ComputeDecode)
  val computeTime = RegInit(0.U(regBits.W))
  val arbiter = (Module(new MyRRArbiter(new SPWriteCmd(scratchType = "Out"), cp.nPE)))
  io.spOutWrite <> arbiter.io.out
  val peArray = for (i <- 0 until cp.nPE) yield {
    Module(new PECSR)
  } 

  var peAllFree = (for(i <- 0 until cp.nPE) yield {
    peArray(i).io.free
  }).reduce(_&&_)
 

  // state machine
  val sIdle :: sAssign :: sWait :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val start = inst_q.io.deq.fire
  val ctr = RegInit(0.U(5.W))

  val inst = RegEnable(inst_q.io.deq.bits, start)
  val blockSizeBytes = (cp.blockSize/8)
  val currRow = RegInit(0.U(32.W))
  val assignStart = (((state === sIdle) && start)
                    || (state === sAssign))
  val rowRem = dec.io.ySizeSp - currRow

  assert (dec.io.ySizeSp =/= 9.U)


// Assigns the lowest row index to the first available PE , 2nd lowest row index to next 
// available PE and so on during the same clock cycle. Sets the valid high according to
// number of higher priority PE that are free and number of rows remaining to be assigned


  for(i <- 0 until cp.nPE){
    peArray(i).io.peReq.bits.denXSize := dec.io.xSizeDen
    peArray(i).io.peReq.bits.spaYSize := dec.io.ySizeSp
    peArray(i).io.peReq.bits.sramColVal := dec.io.sramColVal
    peArray(i).io.peReq.bits.sramDen := dec.io.sramDen
    peArray(i).io.peReq.bits.sramPtr := dec.io.sramPtr
    peArray(i).io.peReq.valid := start
    peArray(i).io.peReq.bits.rowIdx := i.U
    peArray(i).io.spWrite <> io.spWrite
    arbiter.io.in(i) <> peArray(i).io.spOutWrite
    io.ecnt((4*i)+1) <> peArray(i).io.ecnt(0)
    io.ecnt((4*i)+2) <> peArray(i).io.ecnt(1)
    io.ecnt((4*i)+3) <> peArray(i).io.ecnt(2)
    io.ecnt((4*i)+4) <> peArray(i).io.ecnt(3)
  }
  val done = RegInit(false.B)
  io.done := done

//   // instruction queue
  dec.io.inst := Mux(start, inst_q.io.deq.bits, inst)

  // control
  switch(state) {
    is(sIdle) {
      done := false.B
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

  // Compute execution time
  when(done){
    computeTime := 0.U
  }.elsewhen(start || computeTime =/= 0.U){
    computeTime := computeTime + 1.U
  }

  io.ecnt(0).bits := computeTime
  io.ecnt(0).valid := done
}
