package nous.kernels

import nous.implicits._
import nous.linalg.Matrix
import spire.math.Numeric

object matrix {

  private[nous] def dot[A](m1: Matrix[A], m2: Matrix[A]) ={
    val ctb = (m: Matrix[A]) => m.rows != 1 && m.cols != 1
    if (ctb(m1) || ctb(m2)) {
      if (m1.isColumnVector && m2.isColumnVector)
        (m1.transpose * m2)(0)
      else if (m1.isColumnVector && m2.isRowVector)
        (m2 * m1)(0)
      else if (m1.isRowVector && m2.isColumnVector)
        (m1 * m2)(0)
      else
        (m1 * m2.transpose)(0)
    } else if (m1.rows == 1 && m2.rows == 1 && m1.cols != 1 && m2.cols != 1) {
      m1 * m2.transpose
    } else if (m1.rows == 1 && m2.cols == 1 && m1.cols != 1 && m2.rows != 1) {
      m1 * m2
    } else if (m1.cols == 1 && m2.rows == 1 && m1.rows != 1 && m2.cols != 1) {
      m2 * m1
    } else if (m1.cols == 1 && m2.cols == 1 && m1.rows != 1 && m2.rows != 1) {
      m1.transpose * m2
    } else
      Predef.implicitly[Numeric[A]].times(m1(0), m2(0))
  }

  private[nous] def blockTranspose[A](inR: Int, inC: Int, in: Array[A], out: Array[A]) = {
    val XOVER = 60

    var r = 0
    val rsz = inR
    val csz = inC
    while (r < rsz) {
      val blockHeight = if (XOVER < rsz - r) XOVER else rsz - r
      var inRow  = r * csz  // first element of current row
      var outCol = r        // first element of current col
      var c = 0
      while (c < csz) {
        val blockWidth = if (XOVER < csz - c) XOVER else csz - c
        val rowEnd = inRow + blockWidth
        while (inRow < rowEnd) {
          var rowSrc = inRow
          var colDst = outCol
          val colEnd = colDst + blockHeight
          while (colDst < colEnd) {
            out(colDst) = in(rowSrc)
            colDst += 1
            rowSrc += csz
          }
          outCol += rsz
          inRow += 1
        }
        c += XOVER
      }
      r += XOVER
    }
  }

  private[nous] def squareTranspose[A](sz: Int, out: Array[A]) = {
    val csz = sz
    val rsz = sz

    var i = 0
    var idx1 = 1
    var cols = csz
    while (i < rsz) {
      var idx2 = (i + 1) * csz + i
      while (idx1 < cols) {
        val v = out(idx1)
        out(idx1) = out(idx2)
        out(idx2) = v
        idx1 += 1
        idx2 += csz
      }
      i += 1
      idx1 += (i + 1)
      cols += csz
    }
  }

}