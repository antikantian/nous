package nous.free

import scala.reflect.ClassTag

import cats._
import nous.kernels.convolution
import nous.kernels.matrix

object interpreters {

  import ast._
  import functors._

  /**
  val defaultBlasInterpreter: BlasOp ~> Id =
    new (BlasOp ~> Id) {
      def apply[A](fa: BlasOp[A]): Id[Trivial] =
        fa match {
          case Gemm(ta, tb, m, n, k, al, a, b, be) =>
            matrix.gemm(ta, tb, m, n, k, al, a, b, be)
            Trivial.catsTrivialInstance
        }
    }

  val defaultConvInterpreter: ConvOp ~> Eval[?] =
    new (ConvOp ~> Eval[?]) {
      def apply[A](fa: ConvOp[A]): Eval[A] =
        fa match {
          case Im2Col(input, c, h, w, k, s, p) => Eval.now(convolution.im2col(input, c, h, w, k, s, p)).map(_.asInstanceOf[A])
        }
    }
   */

}