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
  val M_SRAM_OFFSET_BITS = 16
  val M_DRAM_OFFSET_BITS = 32
  val M_SIZE_BITS = 7

  val C_UOP_BGN_BITS = 13
  val C_UOP_END_BITS = 14
  val C_ITER_BITS = 14
  val C_AIDX_BITS = 11
  val C_IIDX_BITS = 11
  val C_WIDX_BITS = 10
  val C_ALU_DEC_BITS = 2
  val C_ALU_OP_BITS = 3
  val C_ALU_IMM_BITS = 16

  val Y = true.B
  val N = false.B

  val OP_L = 0.asUInt(OP_BITS.W)
  val OP_S = 1.asUInt(OP_BITS.W)
  val OP_G = 2.asUInt(OP_BITS.W)
  val OP_X = 3.asUInt(OP_BITS.W)

}

/** ISA.
 *
 * TODO: Modify to support SparseMM.
 * TODO: Modify load/store/alu/gemm
 */
object ISA {
  private val xLen = 128
  private val depBits = 4

  private val idBits: HashMap[String, Int] =
    HashMap(("task", 3), ("mem", 3), ("alu", 3))

  private val taskId: HashMap[String, String] =
    HashMap(("load", "111"),
      ("store", "001"),
      ("gemm", "000"),
      ("finish", "011"),
      ("alu", "100"))

  private val memId: HashMap[String, String] =
    HashMap(("uop", "000"), ("wgt", "111"), ("inp", "010"), ("acc", "011"), ("out", "100"))

  private val aluId: HashMap[String, String] =
    HashMap(("minpool", "000"),
      ("maxpool", "001"),
      ("add", "010"),
      ("shift", "011"))

  private def dontCare(bits: Int): String = "?" * bits

  private def instPat(bin: String): BitPat = BitPat("b" + bin)

  private def load(id: String): BitPat = {
    val rem = xLen - idBits("mem") - depBits - idBits("task")
    val inst = dontCare(rem) + memId(id) + dontCare(depBits) + taskId("load")
    instPat(inst)
  }

  private def store: BitPat = {
    val rem = xLen - idBits("task")
    val inst = dontCare(rem) + taskId("store")
    instPat(inst)
  }

  private def gemm: BitPat = {
    val rem = xLen - idBits("task")
    val inst = dontCare(rem) + taskId("gemm")
    instPat(inst)
  }

  private def alu(id: String): BitPat = {
    // TODO: move alu id next to task id
    val inst = dontCare(17) + aluId(id) + dontCare(105) + taskId("alu")
    instPat(inst)
  }

  private def finish: BitPat = {
    val rem = xLen - idBits("task")
    val inst = dontCare(rem) + taskId("finish")
    instPat(inst)
  }

  def LUOP = load("uop")
  def LWGT = load("wgt")
  def LINP = load("inp")
  def LACC = load("acc")
  def SOUT = store
  def GEMM = gemm
  def VMIN = alu("minpool")
  def VMAX = alu("maxpool")
  def VADD = alu("add")
  def VSHX = alu("shift")
  def FNSH = finish
}
