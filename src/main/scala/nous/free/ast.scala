package nous.free

import cats._

object ast {

  //---> BLAS Operations <---
  sealed trait BlasOp[A]
  case class Gemm[A](
      transA  : String,
      transB  : String,
      m       : Int,
      n       : Int,
      k       : Int,
      alpha   : A,
      a       : Array[A],
      b       : Array[A],
      beta    : A)
    extends BlasOp[Trivial]

  case class Gemv[A](
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
      incy  : Int)
    extends BlasOp[Trivial]

  case class Scal[A](n: Int, sa: A, sx: Array[A], incx: Int) extends BlasOp[Trivial]

  case class Copy[A](n: Int, sx: Array[A], incx: Int, sy: Array[A], incy: Int) extends BlasOp[Trivial]

  case class Axpy[A](n: Int, sa: A, sx: Array[A], incx: Int, sy: Array[A], incy: Int) extends BlasOp[Trivial]

  case class Dot[A](n: Int, sx: Array[A], incx: Int, sy: Array[A], incy: Int) extends BlasOp[Trivial]
  //---> End BLAS Operations

  //---> Convolution Operations <---
  sealed trait ConvOp[A]
  case class Im2Col[A](
      input     : Vector[A],
      channels  : Int,
      height    : Int,
      width     : Int,
      kernel    : Int,
      stride    : Int,
      pad       : Int)
    extends ConvOp[Array[A]]
  //---> End Convolution Operations <---

}