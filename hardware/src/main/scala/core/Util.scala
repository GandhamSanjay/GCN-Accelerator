package gcn.core

import chisel3._
import chisel3.util._

object MuxTree {
  def apply[T <: Data](idx: UInt, vec: Seq[T]): T = {
    require(vec.size > 0)
    require(idx.getWidth >= log2Ceil(vec.size), s"idx.getWidth=${idx.getWidth} should cover vec.size=${vec.size}")
    if (vec.size == 1) {
      vec(0)
    } else if (vec.size == 2) {
      Mux(idx(0), vec(1), vec(0))
    } else { // vec.size > 2
      val idx_msb  = log2Ceil(vec.size) - 1
      val vec_half = 1 << idx_msb
      Mux(idx(idx_msb), apply(idx(idx_msb - 1, 0), vec.drop(vec_half)), apply(idx(idx_msb - 1, 0), vec.take(vec_half)))
    }
  }
}