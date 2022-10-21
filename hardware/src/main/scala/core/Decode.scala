package gcn.core

import chisel3._
import chisel3.util._

import  ISA._

/** MemDecode.
 *
 * Decode memory instructions with a Bundle. This is similar to an union,
 * therefore order matters when declaring fields. These are the instructions
 * decoded with this bundle:
 *   - LUOP
 *   - LWGT
 *   - LINP
 *   - LACC
 *   - SOUT
 */
class MemDecode extends Bundle{

  val M_DEP_BITS = 4
  val M_ID_BITS = 3
  val M_DRAM_OFFSET_BITS = 32
  val M_SRAM_OFFSET_BITS = 16
  val M_XSIZE_BITS = 7
  val M_YSIZE_BITS = 0
  val OP_BITS = 2

  val empty = UInt(64.W)
  val ysize = UInt(M_YSIZE_BITS.W)
  val xsize = UInt(M_XSIZE_BITS.W)
  val sram_offset = UInt(M_DRAM_OFFSET_BITS.W)
  val dram_offset = UInt(M_SRAM_OFFSET_BITS.W)
  val id = UInt(M_ID_BITS.W)
  val push_next = UInt(1.W)
  val push_prev = UInt(1.W)
  val pop_next = UInt(1.W)
  val pop_prev = UInt(1.W)
  val op = UInt(OP_BITS.W)
}

/** SpMMDecode.
 *
 * Decode GEMM instruction with a Bundle. This is similar to an union,
 * therefore order matters when declaring fields.
 */
class SpMMDecode extends Bundle {
  val C_SRAM_OFFSET_BITS = 16
  val C_XSIZE_BITS = 7
  val C_YSIZE_BITS = 7
  val OP_BITS = 2

  val empty = UInt(58.W)
  val y_size = UInt(C_YSIZE_BITS.W)
  val x_size = UInt(C_XSIZE_BITS.W)
  val sram_offset_den = UInt(C_SRAM_OFFSET_BITS.W)
  val sram_offset_ptr = UInt(C_SRAM_OFFSET_BITS.W)
  val sram_offset_col_val = UInt(C_SRAM_OFFSET_BITS.W)
  val push_next = Bool()
  val push_prev = Bool()
  val pop_next = Bool()
  val pop_prev = Bool()
  val op = UInt(OP_BITS.W)
}

// /** AluDecode.
//  *
//  * Decode ALU instructions with a Bundle. This is similar to an union,
//  * therefore order matters when declaring fields. These are the instructions
//  * decoded with this bundle:
//  *   - VMIN
//  *   - VMAX
//  *   - VADD
//  *   - VSHX
//  */
// class AluDecode extends Bundle {
//   val alu_imm = UInt(C_ALU_IMM_BITS.W)
//   val alu_use_imm = Bool()
//   val alu_op = UInt(C_ALU_OP_BITS.W)
//   val src_1 = UInt(C_AIDX_BITS.W)
//   val src_0 = UInt(C_AIDX_BITS.W)
//   val dst_1 = UInt(C_AIDX_BITS.W)
//   val dst_0 = UInt(C_AIDX_BITS.W)
//   val empty_0 = Bool()
//   val lp_1 = UInt(C_ITER_BITS.W)
//   val lp_0 = UInt(C_ITER_BITS.W)
//   val uop_end = UInt(C_UOP_END_BITS.W)
//   val uop_begin = UInt(C_UOP_BGN_BITS.W)
//   val reset = Bool()
//   val push_next = Bool()
//   val push_prev = Bool()
//   val pop_next = Bool()
//   val pop_prev = Bool()
//   val op = UInt(OP_BITS.W)
// }

// /** UopDecode.
//  *
//  * Decode micro-ops (uops).
//  */
// class UopDecode extends Bundle {
//   val u2 = UInt(10.W)
//   val u1 = UInt(11.W)
//   val u0 = UInt(11.W)
// }

/** FetchDecode.
 *
 * Partial decoding for dispatching instructions to Load, Compute, and Store.
 */
class FetchDecode extends Module with ISAConstants{
  val io = IO(new Bundle {
    val inst = Input(UInt(INST_BITS.W))
    val isLoad = Output(Bool())
    val isCompute = Output(Bool())
    val isStore = Output(Bool())
  })
  val csignals =
    ListLookup(
      io.inst,
      List(N, OP_X),
      Array(
        LCOL -> List(Y, OP_L),
        LPTR -> List(Y, OP_L),
        LVAL -> List(Y, OP_L),
        LDEN -> List(Y, OP_L),
        SPMM -> List(Y, OP_C)
      )
    )

  val (cs_val_inst: Bool) :: cs_op_type :: Nil = csignals

  io.isLoad := (cs_val_inst && cs_op_type === OP_L)
  io.isCompute := (cs_val_inst & cs_op_type === OP_C)
  io.isStore := (cs_val_inst & cs_op_type === OP_S)
}

/** LoadDecode.
 *
 * Decode dependencies, type and sync for Load module.
 */
class LoadDecode extends Module with ISAConstants{
  val io = IO(new Bundle {
    val inst = Input(UInt(INST_BITS.W))
    // val push_next = Output(Bool())
    // val pop_next = Output(Bool())
    val isSeq = Output(Bool())
    val isVal = Output(Bool())
    val isCol = Output(Bool())
    val isPtr = Output(Bool())
    val xSize = Output(UInt(M_XSIZE_BITS.W))
    val ySize = Output(UInt(M_YSIZE_BITS.W))
    val dramOffset = Output(UInt(M_DRAM_OFFSET_BITS.W))
    val sramOffset = Output(UInt(M_SRAM_OFFSET_BITS.W))
  })
  val dec = io.inst.asTypeOf(new MemDecode)
  io.isSeq := io.isVal || io.isCol || io.isPtr
  io.isVal := io.inst === LVAL
  io.isCol := io.inst === LCOL
  io.isPtr := io.inst === LPTR
  io.xSize := dec.xsize
  io.ySize := dec.ysize
  io.sramOffset := dec.sram_offset
  io.dramOffset := dec.dram_offset
}

/** ComputeDecode.
 *
 * Decode dependencies, type and sync for Compute module.
 */
class ComputeDecode extends Module with ISAConstants{
  val io = IO(new Bundle {
    val inst = Input(UInt(INST_BITS.W))
    val sramColVal = Output(UInt(C_SRAM_OFFSET_BITS.W))
    val sramPtr = Output(UInt(C_SRAM_OFFSET_BITS.W))
    val sramDen = Output(UInt(C_SRAM_OFFSET_BITS.W))
    val xSizeDen = Output(UInt(C_XSIZE_BITS.W))
    val ySizeSp = Output(UInt(C_YSIZE_BITS.W))
  })
  val dec = io.inst.asTypeOf(new SpMMDecode)
  io.sramColVal := dec.sram_offset_col_val
  io.sramPtr := dec.sram_offset_ptr
  io.sramDen := dec.sram_offset_den
  io.xSizeDen := dec.x_size
  io.ySizeSp := dec.y_size
}

// /** StoreDecode.
//  *
//  * Decode dependencies, type and sync for Store module.
//  */
// class StoreDecode extends Module {
//   val io = IO(new Bundle {
//     val inst = Input(UInt(INST_BITS.W))
//     val push_prev = Output(Bool())
//     val pop_prev = Output(Bool())
//     val isStore = Output(Bool())
//     val isSync = Output(Bool())
//   })
//   val dec = io.inst.asTypeOf(new MemDecode)
//   io.push_prev := dec.push_prev
//   io.pop_prev := dec.pop_prev
//   io.isStore := io.inst === SOUT & dec.xsize =/= 0.U
//   io.isSync := io.inst === SOUT & dec.xsize === 0.U
// }
