package nous.free

import cats._
import cats.free.Free
import nous.network.definitions.ConvDefinition
import nous.network.layers.{Activation, LambdaInjection, WeightInit}

object functors {

  import ast._

  //---> BLAS Functors <---
  type BlasF[A] = Free[BlasOp, A]
  def gemm[A](
      transA  : String,
      transB  : String,
      m       : Int,
      n       : Int,
      k       : Int,
      alpha   : A,
      a       : Array[A],
      b       : Array[A],
      beta    : A): BlasF[Trivial] = {
    Free.liftF[BlasOp, Trivial](Gemm(transA, transB, m, n, k, alpha, a, b, beta))
  }

  def gemv[A](
      trans : String,
      m     : Int,
      n     : Int,
      alpha : A,
      a     : Array[A],
      lda   : Int,
      x     : Array[A],
      incx  : Int,
      beta  : A,
      y     : Array[A],
      incy  : Int): BlasF[Trivial] = {
    Free.liftF[BlasOp, Trivial](Gemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy))
  }

  def scal[A](n: Int, sa: A, sx: Array[A], incx: Int): BlasF[Trivial] =
    Free.liftF[BlasOp, Trivial](Scal(n, sa, sx, incx))

  def copy[A](n: Int, sx: Array[A], incx: Int, sy: Array[A], incy: Int): BlasF[Trivial] =
    Free.liftF[BlasOp, Trivial](Copy(n, sx, incx, sy, incy))

  def axpy[A](n: Int, sa: A, sx: Array[A], incx: Int, sy: Array[A], incy: Int): BlasF[Trivial] =
    Free.liftF[BlasOp, Trivial](Axpy(n, sa, sx, incx, sy, incy))

  def dot[A](n: Int, sx: Array[A], incx: Int, sy: Array[A], incy: Int): BlasF[Trivial] =
    Free.liftF[BlasOp, Trivial](Dot(n, sx, incx, sy, incy))
  //---> End BLAS Functors <---

  //---> Conv Functors <---
  type ConvF[A] = Free[ConvOp, A]
  def im2col[A](
      input     : Vector[A],
      channels  : Int,
      height    : Int,
      width     : Int,
      kernel    : Int,
      stride    : Int,
      pad       : Int): ConvF[Array[A]] = {
    Free.liftF[ConvOp, Array[A]](Im2Col(input, channels, height, width, kernel, stride, pad))
  }
  //---> End Conv Functors <---

  //---> Layer Def Functors <---
  type LayerDefF[A] = Free[LayerDefOp, A]
  def convolution[A](
      filters         : Int,
      height          : Int,
      width           : Int,
      stride          : Int,
      padding         : Int,
      bias            : Boolean,
      initialization  : WeightInit[A],
      activation      : Activation[A],
      lambda          : Option[LambdaInjection[A]] = None): LayerDefF[ConvDefinition[A]] = {
    Free.liftF[LayerDefOp, ConvDefinition[A]] {
      ConvolutionLayer(filters, height, width, stride, padding, bias, initialization, activation, lambda)
    }
  }
  //---> End Layer Def Functors <---

}