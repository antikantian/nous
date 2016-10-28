package nous.syntax.ops

import com.github.fommil.netlib.BLAS.{ getInstance => natblas }
import nous.data.matrix._

object TypedMatrixOps {

  private[nous] def transposed(m: Matrix[_]): String =
    if (m.isTransposed) "T" else "N"

  /**
  final class FloatMatrixOps(lhs: Matrix[Float])(implicit ev: Matrix[Float]) {
    def *(rhs: Matrix[Float]): Matrix[Float] = {
      (lhs.isTransposed, rhs.isTransposed) match {
        case (true, true) => assert(lhs.rows == rhs.cols, "Invalid dimensions: a.rows != b.cols")
        case (false, true) => assert(lhs.cols == rhs.cols, "Invalid dimensions: a.cols != b.cols")
        case (true, false) => assert(lhs.rows == rhs.rows, "Invalid dimensions: a.rows != b.rows")
        case _ => assert(lhs.cols == rhs.rows, "Invalid dimensions: a.cols != b.rows")
      }

      val transa = transposed(lhs)
      val transb = transposed(rhs)
      val k = if (rhs.isTransposed) rhs.cols else rhs.rows
      val result = new Array[Float](lhs.rows * k)
      natblas.sgemm(transa, transb, lhs.rows, rhs.cols, k, 1, lhs.toArray, lhs.rows, rhs.toArray, rhs.rows, 0, result, lhs.rows)
      result
    }
  }
   */

}