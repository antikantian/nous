package nous.free

import cats.free.Free
import nous.data._
import spire.math.Numeric

object tensor {

  /**

  sealed trait TensorOp[A]

  object TensorOp {

    case class Convolve[A](
        strideXY  : (Int, Int),
        paddingXY : (Int, Int),
        weights   : Tensor[A],
        filters   : Int,
        channels  : Int,
        rows      : Int,
        cols      : Int,
        input     : Tensor[A]) extends TensorOp[Tensor[A]]
  }

  import TensorOp._

  type TensorIO[A] = Free[TensorOp, A]

  def convolve[A: Numeric](
      strideXY  : (Int, Int),
      paddingXY : (Int, Int),
      weights   : Tensor[A],
      filters   : Int,
      channels  : Int,
      rows      : Int,
      cols      : Int,
      input     : Tensor[A]): TensorIO[Tensor[A]] = {
    Free.liftF[TensorOp, Tensor[A]](Convolve(strideXY, paddingXY, weights, filters, channels, rows, cols, input))
  }

  */


}