package nous.linalg

import scala.annotation.tailrec

import cats.{ Applicative, Eval, MonadState, Show }
import cats.data.State
import fs2.Task
import nous.kernels.matrix._
import nous.linalg.Coord.{ Point, PointWithVal => PwV }
import spire.algebra._
import spire.math._

class Matrix[A](val m: Int, val n: Int)(values: Array[A]) { self =>

  val data = values

  val rows: Int = m

  val cols: Int = n

  val cachedT = Eval.later {
    val arrT = data.clone

    if (self.isSquare)
      squareTranspose(cols, arrT)
    else
      blockTranspose(rows, cols, self.toArray, arrT)

    new Matrix(cols, rows)(arrT)
  }

  def length: Int = rows * cols

  def isSquare: Boolean = rows == cols

  def isEmpty: Boolean = length == 0

  def isColumnVector: Boolean = cols == 1

  def isRowVector: Boolean = rows == 1

  def isVector: Boolean = isColumnVector || isRowVector

  def isFinite(implicit ev: NumberTag[A]): Boolean = forall(ev.isFinite)

  def forall(f: A => Boolean): Boolean = data forall f

  def at(r: Int, c: Int): A = raw(r, c)

  def raw(i: Int): A = apply(i)

  def raw(r: Int, c: Int): A = apply(r, c)

  def flatMap[B](fb: A => Matrix[B]): Matrix[B]

  def map[B](f: A => B): Matrix[B] = new Matrix[B](m, n)(data map f)

  def reshape(r: Int, c: Int): Matrix[A]

  def transpose: Matrix[A] = cachedT.value

  def T = transpose

  def col(c: Int): Vector[A]

  def row(r: Int): Vector[A]

  def mult[B](m: Matrix[B]): Matrix[Double]

  def roundTo(sig: Int): Matrix[Double]

  def toVector: Vector[A]

  // Utility functions
  def isRowMajor: Boolean

  /** Returns a column vector R that contains the elements from the diagonal of m in
   *  the order R(0) == m(0, 0), R(1) == m(1, 1), R(2) == m(2, 2) etc. */
  def diag: ColumnVector[A]

  def lowerm: Matrix[A]

  def lowerm(scalar: A): Matrix[A]

  def upperm: Matrix[A]

  def upperm(scalar: A): Matrix[A]

  def symmetric: Matrix[A]

  def rotate(r: Int, c: Int): Matrix[A]

  def flipLeftRight: Matrix[A]

  def flipUpDown: Matrix[A]

  def flip: Matrix[A]

  def remove(row: Int, col: Int): Matrix[A]

  def removeRow(i: Int): Matrix[A]

  def removeCol(i: Int): Matrix[A]

  def lowerbound(threshold: A): Matrix[A]

  def upperbound(threshold: A): Matrix[A]

  def max: A = data reduceLeft { (b, a) =>
    Predef.implicitly[Order[A]].max(a, b)
  }

  def min: A = data reduceLeft { (b, a) =>
    Predef.implicitly[Order[A]].min(a, b)
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

  def maxPoint(implicit ev: Order[A]): Point = {
    val pv = reduceLeftP { (pv1, pv2) => if (ev.gt(pv1.a, pv2.a)) pv1 else pv2 }
    Point(pv.x, pv.y)
  }

  def minPoint(implicit ev: Order[A]): Point = {
    val pv = reduceLeftP { (pv1, pv2) => if (ev.lt(pv1.a, pv2.a)) pv1 else pv2 }
    Point(pv.x, pv.y)
  }

  def sum: Matrix[A]

  def sumRows: RowVector[A]

  def sumCols: ColumnVector[A]

  def prod: Matrix[A]

  def mean: Matrix[A]

  def variance: Matrix[A]

  def stddev: Matrix[A]

  def covariance: Matrix[A]

  def reduceLeftP(f: (PwV[A], PwV[A]) => PwV[A]): PwV[A] = {
    @tailrec
    def inner(phead: PwV[A], ptail: Seq[PwV[A]]): PwV[A] = ptail match {
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

  def pointsV: IndexedSeq[PwV[A]] =
    for {
      ps <- points.value
      pa <- PwV(ps.x, ps.y, at(ps.x, ps.y))
    } yield pa

  private[nous] def apply(i: Int): A

  private[nous] def apply(r: Int, c: Int): A

  private[nous] def toArray: Array[A]

  private var flatCache: Option[Vector[A]] = None
  private[nous] def flatten: Vector[A] = flatCache.getOrElse {
    self.synchronized {
      flatCache = Some(toVector)
      flatCache.get
    }
  }

  private var flatCacheT: Option[Vector[A]] = None
  private[nous] def flattenT: Vector[A] = flatCacheT.getOrElse {
    self.synchronized {
      flatCacheT = Some(T.toVector)
      flatCacheT.get
    }
  }

}

object Matrix {

  def random[A](rows: Int, cols: Int): Matrix[A]

  def gaussian[A](rows: Int, cols: Int, seed: Long = 0): Matrix[A]

  def uniform[A](rows: Int, cols: Int, a: A): Matrix[A]

  def ones[A](m: Matrix[A]): Matrix[A]

  def ones[A](rows: Int, cols: Int): Matrix[A]

  def zeros[A](m: Matrix[A]): Matrix[A]

  def zeros[A](rows: Int, cols: Int): Matrix[A]

  def ident[A](m: Matrix[A]): Matrix[A]

  def ident[A](n: Int): Matrix[A]

  def linspace[A](start: A, end: A, num: Int): RowVector[A]

  def logspace[A](start: A, end: A, num: Int): RowVector[A]

  def ewMultiply[A](m1: Matrix[A], m2: Matrix[A]): Matrix[A]

  def joinCols[A](m1: Matrix[A], m2: Matrix[A]): Matrix[A]

  def tensorProduct[A](m1: Matrix[A], m2: Matrix[A]): Matrix[A]

  def cartesianProduct[A](m1: Matrix[A], m2: Matrix[A]): Matrix[A]

  def scaleRows[A](m: Matrix[A], v: Vector[A]): Matrix[A]

  def sortCols[A](m: Matrix[A], v: ColumnVector[A]): Task[Unit]

}

sealed abstract class MatrixInstances {
  implicit def matrixCanShow[A]: Show[Matrix[A]] = new Show[Matrix[A]] {
    def show(mat: Matrix[A], srows: Int = 8, scols: Int = 8): String = {
      val halfr = srows / 2
      val halfc = scols / 2
      val maxStrLen = (a: Int, b: String) => a.max(b.length)

      val M = MonadState[State[StringBuilder, String], StringBuilder]
      import M._

      for {
        _ <- modify(_.append(s"[${mat.rows} x ${mat.cols}]\n"))
        i <- inspect(_.toString)
      } yield i
    }
  }

  implicit val matrixIsApplicative: Applicative[Matrix] = new Applicative[Matrix] {
    def pure[A](a: A): Matrix[A] =
      new Matrix[A](1, 1)(Array(a))

    def ap[A, B](f: Matrix[A => B])(fa: Matrix[A]): Matrix[B] =
      fa.flatMap { a => f.map(ff => ff(a)) }
  }

}