package gcn.core

import chisel3._
import chisel3.util._
import scala.collection.mutable.HashMap

/** ISAConstants.
 *
 * These constants are used for decoding (parsing) fields on instructions.
 */
trait ISAConstants {
  val INST_BITS = 128

  val OP_BITS = 2

  val M_DEP_BITS = 4
  val M_ID_BITS = 3
  val M_DRAM_OFFSET_BITS = 32
  val M_SRAM_OFFSET_BITS = 16
  val M_XSIZE_BITS = 7
  val M_YSIZE_BITS = 0

  val C_DEP_BITS = 4
  val C_SRAM_OFFSET_BITS = 16
  val C_XSIZE_BITS = 7
  val C_YSIZE_BITS = 7

  val Y = true.B
  val N = false.B

  val OP_L = 0.asUInt(OP_BITS.W)
  val OP_S = 1.asUInt(OP_BITS.W)
  val OP_C = 2.asUInt(OP_BITS.W)
  val OP_X = 3.asUInt(OP_BITS.W)

}

/** ISA.
 *
 * TODO: Modify to support SparseMM.
 * TODO: Modify load/store/alu/gemm
 */
object ISA {
  val INST_BITS = 128
  val OP_BITS = 2
  val M_DEP_BITS = 4
  val M_ID_BITS = 3
  val M_DRAM_OFFSET_BITS = 32
  val M_SRAM_OFFSET_BITS = 16
  val M_XSIZE_BITS = 7
  val M_YSIZE_BITS = 0
  val C_DEP_BITS = 4
  val C_SRAM_OFFSET_BITS = 16
  val C_XSIZE_BITS = 7
  val C_YSIZE_BITS = 0
  val Y = true.B
  val N = false.B
  val OP_L = 0.asUInt(OP_BITS.W)
  val OP_S = 1.asUInt(OP_BITS.W)
  val OP_C = 2.asUInt(OP_BITS.W)
  val OP_X = 3.asUInt(OP_BITS.W)

  private val xLen = 128
  private val depBits = 4

  private val idBits: HashMap[String, Int] =
    HashMap(("task", 2), ("mem", 2))

  private val taskId: HashMap[String, String] =
    HashMap(("load", "00"),
      ("store", "01"),
      ("spmm", "10"),
      ("finish", "11"))

  private val memId: HashMap[String, String] =
    HashMap(("col", "000"), ("ptr", "001"), ("val", "010"), ("den", "011"), ("out", "100"))

  private def dontCare(bits: Int): String = "?" * bits

  private def instPat(bin: String): BitPat = BitPat("b" + bin)

  private def load(id: String): BitPat = {
    val rem = xLen - idBits("mem") - depBits - idBits("task")
    val inst = dontCare(rem) + memId(id) + dontCare(depBits) + taskId("load")
    instPat(inst)
  }

  private def spmm: BitPat = {
    val rem = xLen - idBits("task")
    val inst = dontCare(rem) + taskId("spmm")
    instPat(inst)
  }


  def LCOL = load("col")
  def LPTR = load("ptr")
  def LVAL = load("val")
  def LDEN = load("den")
  def LOUT = load("out")
  def SPMM = spmm

  val scratchID = 
      HashMap(("Col", 8), ("Val", 1), ("Ptr", 4), ("Den", 2))
  
}
