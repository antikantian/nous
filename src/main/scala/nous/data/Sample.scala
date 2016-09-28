package nous.data

import scala.{Vector => SVector}

import cats._
import cats.data.{Xor, NonEmptyVector => Nev}
import nous.util.exception._
import spire.algebra.Eq

/**

class Sample[A](k: Int, nr: Int, nc: Int)(private[nous] val values: Array[A], val num: Int = -1) { self =>
  val number = num
  val rows = nr
  val cols = nc
  val depth = k
  val size = rows * cols * depth
  val entries = Eval.later { values.sliding(rows * cols).map(arr => Matrix.fromArray(rows, cols, arr)).toList }

  def head = entries.value.head
  def tail = entries.value.tail

  def raw(channel: Int, row: Int, col: Int): A =
    values.apply { ((depth + channel) * rows + row) * cols + col }

  def indexOf(channel: Int, row: Int, col: Int): Int =
    ((depth + channel) * rows + row) * cols + col

  /** Map over each value in each entry in this sample */
  def map[B](f: A => B): Sample[B] =
    new Sample(k, nr, nc)(values.map(f), number)

  /** Map over each entry in this sample */
  def mapEntries[B](f: Matrix[A] => Matrix[B]): Sample[B] = {
    val mapped = entries.value.map(f).foldLeft(new Array[B](size)) { (arr, mat) => arr ++ mat.data }
    new Sample(k, nr, nc)(mapped, number)
  }

  def renumber(n: Int) = new Sample[A](k, nr, nc)(values, n)

  def boundary: Rectangle = Rectangle(0, 0, cols - 1, rows - 1)

  def asMatrix: Matrix[A] = new Matrix[A](depth, rows * cols)

}

object Sample extends SampleInstances {

  def apply[A](k: Int, nr: Int, nc: Int, data: Array[A]): Sample[A] =
    new Sample(k, nr, nc)(data)

  def apply[A](k: Int, nr: Int, nc: Int, sampleNum: Int, data: Array[A]): Sample[A] =
    new Sample(k, nr, nc)(data, sampleNum)

  def dimsMatch[A](s1: Sample[A], s2: Sample[A]): SampleMismatch Xor Sample[A] =
    if (implicitly[Eq[Sample[A]]].eqv(s1, s2))
      Xor.Right(s2)
    else {
      val mismatchmsg =
        s"""
          | Sample(${s1.num}) of size (${s1.rows}, ${s1.cols}, ${s1.depth}) does not match Sample(${s2.num}) of
          | size (${s2.rows}, ${s2.cols}, ${s2.depth}).
        """.stripMargin
      Xor.Left(SampleMismatch(mismatchmsg))
    }

  def dimsMatchThrow[A](s1: Sample[A], s2: Sample[A]): Sample[A] =
    dimsMatch(s1, s2) match {
      case Xor.Right(sample) => sample
      case Xor.Left(except) => throw except
    }

}

sealed abstract class SampleInstances {

  implicit def sampleCanEq[A]: Eq[Sample[A]] = new Eq[Sample[A]] {
    def eqv(x: Sample[A], y: Sample[A]): Boolean = {
      x.rows == y.rows && x.cols == y.cols && x.depth == y.depth
    }
  }

  implicit def sampleIsMonoid[A]: Monoid[Sample[A]] = new Monoid[Sample[A]] {
    def combine(x: Sample[A], y: Sample[A]): Sample[A] =
      new Sample(x.entries ++ y.entries)(x.number)

    def empty = new Sample(List.empty[Matrix[A]])

  }

}

*/