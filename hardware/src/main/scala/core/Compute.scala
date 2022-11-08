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
  val cr = p(AccKey).crParams
  val regBits = p(AccKey).crParams.regBits
  val io = IO(new Bundle {
    val inst = Flipped(Decoupled(UInt(INST_BITS.W)))
    val spWrite = Vec(cp.nScratchPadMem, Flipped(Decoupled(new SPWriteCmd)))
    val valid = Input(Bool())
    val done = Output(Bool())
    val spOutWrite = Decoupled(new SPWriteCmd(scratchType = "Col"))
    val ecnt = Output(Vec(p(AccKey).crParams.nComputeEventCtr, UInt(regBits.W)))
  })

  // Module instantiation
  val inst_q = Module(new Queue(UInt(INST_BITS.W), cp.computeInstQueueEntries))
  val dec = Module(new ComputeDecode)
  val computeTime = RegInit(0.U(regBits.W))
  val arbiter = (Module(new MyRRArbiter(new SPWriteCmd(scratchType = "Out"), cp.nPE)))
  io.spOutWrite <> arbiter.io.out
  val peArray = for (i <- 0 until cp.nPE) yield {
    Module(new PipelinedPECSR)
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
  val rowRem = dec.io.ySizeSp
  val rowAssignment =  RegInit(VecInit(Seq.fill(cp.nPE)(0.U(cp.blockSize.W))))
  val peRowsDone = Wire(Vec(cp.nPE, Bool()))
  val allPeDone = peRowsDone.reduce(_&&_)
  assert (dec.io.ySizeSp =/= 9.U)


  for(i <- 0 until cp.nPE){
    when(start){
      peArray(i).io.peReq.valid := true.B
      peArray(i).io.peReq.bits.rowIdx := i.U
      rowAssignment(i) := (i + cp.nPE).U
    }.otherwise{
      peArray(i).io.peReq.valid := !peRowsDone(i)
      peArray(i).io.peReq.bits.rowIdx := rowAssignment(i)
      when(peArray(i).io.peReq.fire){
        rowAssignment(i) := rowAssignment(i) + cp.nPE.U
      }
    }
    peRowsDone(i) := rowAssignment(i) >= dec.io.ySizeSp
    peArray(i).io.peReq.bits.denXSize := dec.io.xSizeDen
    peArray(i).io.peReq.bits.spaYSize := dec.io.ySizeSp
    peArray(i).io.peReq.bits.sramColVal := dec.io.sramColVal
    peArray(i).io.peReq.bits.sramDen := dec.io.sramDen
    peArray(i).io.peReq.bits.sramPtr := dec.io.sramPtr
    peArray(i).io.spWrite <> io.spWrite
    arbiter.io.in(i) <> peArray(i).io.spOutWrite
    for( j <- 0 until cr.nPEEventCtr){
      io.ecnt((cr.nPEEventCtr*i)+j+1) <> peArray(i).io.ecnt(j)
    }
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
        state := sAssign
      }
    }
    is(sAssign){
      when(allPeDone){
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

  io.ecnt(0) := computeTime
}
