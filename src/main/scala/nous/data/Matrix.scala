package nous.data

import scala.{ Vector => SVector }
import scala.annotation.tailrec
import scala.reflect.ClassTag

import cats.{Applicative, Eval, MonadState, Show}
import cats.data.State
import fs2.Task
import nous.kernels.matrix._
import spire.algebra._
import spire.math._
/**
/** Assumes row-major order */
class Matrix[A](val m: Int, val n: Int)(values: Array[A], private[nous] val isTransposed: Boolean = false) { self =>

  import Coord._

  val data = values

  val rows: Int = m

  val cols: Int = n

  def length: Int = rows * cols

  def isSquare: Boolean = rows == cols

  def isEmpty: Boolean = length == 0

  def isColumnVector: Boolean = cols == 1

  def isRowVector: Boolean = rows == 1

  def isVector: Boolean = isColumnVector || isRowVector

  def isFinite(implicit ev: NumberTag[A]): Boolean = forall(ev.isFinite)

  def isRowMajor: Boolean = !self.isTransposed

  def isColumnMajor: Boolean = !isRowMajor

  /** Matrix-matrix addition, returns a new matrix R where R(r,c) == this(r,c) + m2(r,c) */
  def +(m2: Matrix[A])(implicit ev: Numeric[A], ct: ClassTag[A]): Matrix[A] = {
    require(rows == m2.rows && cols == m2.cols, "Cannot add matrices with different dimensions.")
    val zipped = pointsV.zip(m2.pointsV) map { p1p2 =>
      val (p1, p2) = p1p2
      ev.plus(p1.a, p2.a)
    }
    new Matrix(m, n)(zipped.toArray)
  }

  /** In-place addition, returns this. */
  def +=(m2: Matrix[A])(implicit ev: Numeric[A]): Matrix[A] = {
    require(rows == m2.rows && cols == m2.cols, "Cannot add matrices with different dimensions.")
    pointsV.zip(m2.pointsV) foreach { p1p2 =>
      val (p1, p2) = p1p2
      update(p1.x, p1.y)(ev.plus(p1.a, p2.a))
    }
    self
  }

  def forall(f: A => Boolean): Boolean = data forall f

  def raw(i: Int): A = apply(i)

  def raw(r: Int, c: Int): A = apply(r, c)

  def at(r: Int, c: Int): A = raw(r, c)

  def indexOf(r: Int, c: Int): Int = r * cols + c

  def update(r: Int, c: Int)(a: A): Unit = data.update(indexOf(r, c), a)

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    data.foldLeft(b)(f)

  def map[B: ClassTag](f: A => B): Matrix[B] = new Matrix[B](m, n)(data map f)

  def reshape(r: Int, c: Int): Matrix[A] = new Matrix(r, c)(data)

  val cachedT = Eval.later {
    val arrT = data.clone

    if (self.isSquare)
      squareTranspose(cols, arrT)
    else
      blockTranspose(rows, cols, self.toArray, arrT)

    new Matrix(cols, rows)(arrT, true)
  }

  def transpose: Matrix[A] = cachedT.value

  def T = transpose

  def toScalaVector: SVector[A] = data.toVector

  def max(implicit ev: Numeric[A]): A = data reduceLeft { (b, a) =>
    ev.max(a, b)
  }

  def min(implicit ev: Numeric[A]): A = data reduceLeft { (b, a) =>
    ev.min(a, b)
  }

  def maxIdx(implicit ev: Order[A]): Int = {
    val di = data.zipWithIndex reduceLeft { (b, a) =>
      val (av, ai) = a
      val (bv, bi) = b
      if (ev.gt(av, bv)) (av, ai) else (bv, bi)
    }
    di._2
  }

  def minIdx(implicit ev: Order[A]): Int = {
    val di = data.zipWithIndex reduceLeft { (b, a) =>
      val (av, ai) = a
      val (bv, bi) = b
      if (ev.lt(av, bv)) (av, ai) else (bv, bi)
    }
    di._2
  }

  // TODO: maxPoint, minPoint, reduceLeftP, switch to fs2
  def maxPoint(implicit ev: Order[A]): Point = {
    val pv = reduceLeftP { (pv1, pv2) => if (ev.gt(pv1.a, pv2.a)) pv1 else pv2 }
    Point(pv.x, pv.y)
  }

  def minPoint(implicit ev: Order[A]): Point = {
    val pv = reduceLeftP { (pv1, pv2) => if (ev.lt(pv1.a, pv2.a)) pv1 else pv2 }
    Point(pv.x, pv.y)
  }

  def reduceLeftP(f: (PointWithVal[A], PointWithVal[A]) => PointWithVal[A]): PointWithVal[A] = {
    @tailrec
    def inner(phead: PointWithVal[A], ptail: Seq[PointWithVal[A]]): PointWithVal[A] = ptail match {
      case IndexedSeq(is, isx) => f(is, isx)
      case IndexedSeq(is, isx@_*) =>
        inner(f(is, isx.head), isx.tail)
    }
    val pv = pointsV
    inner(pv.head, pv.tail)
  }

  val points: Eval[IndexedSeq[Point]] = Eval.later {
    for {
      r <- 0 until rows
      c <- 0 until cols
    } yield Point(r, c)
  }

  def pointsV: IndexedSeq[PointWithVal[A]] =
    for {
      ps <- points.value
    } yield PointWithVal(ps.x, ps.y, at(ps.x, ps.y))

  def asRectangle: Rectangle = Rectangle(0, 0, cols - 1, rows - 1)

  private[nous] def apply(i: Int): A = data(i)

  private[nous] def apply(r: Int, c: Int): A = apply(r * cols + c)

  private[nous] def toArray: Array[A] = data

  private var flatCache: Option[SVector[A]] = None
  private[nous] def flatten: SVector[A] = flatCache.getOrElse {
    self.synchronized {
      flatCache = Some(toScalaVector)
      flatCache.get
    }
  }

  private var flatCacheT: Option[SVector[A]] = None
  private[nous] def flattenT: SVector[A] = flatCacheT.getOrElse {
    self.synchronized {
      flatCacheT = Some(T.toScalaVector)
      flatCacheT.get
    }
  }

}

object Matrix extends MatrixInstances {

  def apply[A](rows: Int, cols: Int, data: Array[A]): Matrix[A] = new Matrix(rows, cols)(data)

  def empty[A: ClassTag]: Matrix[A] = new Matrix(0, 0)(new Array[A](0))

  def fromArray[A](rows: Int, cols: Int, arr: Array[A]): Matrix[A] = apply(rows, cols, arr)

  def fromTensor[A](t: Tensor[A], rows: Int, cols: Int) = {
    require(rows * cols == t.data.length, "Invalid size for conversionto matrix")
    new Matrix(rows, cols)(t.data.toArray)
  }

  def fromTensor[A: ClassTag](t: Tensor[A]): Matrix[A] =
    if (t.nonEmpty) fromTensor(t, t.samples.value, t.size / t.samples.value) else empty[A]

  def ones[A: ClassTag](rows: Int, cols: Int)(implicit ev: Numeric[A]): Matrix[A] =
    new Matrix(rows, cols)(Array.fill(rows * cols)(ev.one))

  def zeros[A: ClassTag](rows: Int, cols: Int)(implicit ev: Numeric[A]): Matrix[A] =
    new Matrix(rows, cols)(Array.fill(rows * cols)(ev.zero))

}

abstract sealed class MatrixInstances {

}

 */
