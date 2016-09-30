package nous.data

import scala.collection.mutable
import scala.{Vector => SVector}
import scala.reflect.ClassTag

import breeze.linalg.DenseMatrix
import breeze.storage.Zero
import cats._
import cats.data._
import fs2._
import nous.util.exception._
import spire.algebra.Eq
import spire.math._

class Sample[A](
    k  : Int,
    nr : Int,
    nc : Int)(
    private[nous]
    val values : Array[A],
    val num    : Int = -1) { self =>

  val data = values
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
  def map[B: ClassTag](f: A => B): Sample[B] =
    new Sample(k, nr, nc)(values.map(f), number)

  /** Map over each entry in this sample */
  def mapEntries[B: ClassTag](f: Matrix[A] => Matrix[B]): Sample[B] = {
    val mapped = entries.value.map(f).foldLeft(new Array[B](size)) { (arr, mat) => arr ++ mat.data }
    new Sample(k, nr, nc)(mapped, number)
  }

  def renumber(n: Int) = new Sample[A](k, nr, nc)(values, n)

  def boundary: Rectangle = Rectangle(0, 0, cols - 1, rows - 1)

  def asMatrix: Matrix[A] = new Matrix[A](depth, rows * cols)(data)

  override def toString = {
    Stream
      .emits(data)
      .vectorChunkN(cols)
      .map(vec => s"""${vec.take(3).mkString(" ")}, ... , ${vec.takeRight(3).mkString(" ")}""")
      .fold(new mutable.StringBuilder())((builder, str) => builder ++= s"$str\n")
      .toVector
      .head
      .toString
  }

}

object Sample extends SampleInstances {

  def apply[A](k: Int, nr: Int, nc: Int, data: Array[A]): Sample[A] =
    new Sample(k, nr, nc)(data)

  def apply[A](k: Int, nr: Int, nc: Int, sampleNum: Int, data: Array[A]): Sample[A] =
    new Sample(k, nr, nc)(data, sampleNum)

  def empty[A: ClassTag](k: Int, nr: Int, nc: Int): Sample[A] =
    new Sample(k, nr, nc)(new Array[A](k * nr * nc))

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

  def fromImage[A: ClassTag: Numeric](x: Image): Sample[A] = {
    val d = x.depth
    val r = x.height
    val c = x.width
    val rgbstream = x.red[A] ++ x.green[A] ++ x.blue[A]
    new Sample[A](d, r, c)(rgbstream.toVector.toArray)
  }

}

sealed abstract class SampleInstances {

  implicit def sampleHasZero[A](implicit ev: Numeric[A]): Zero[A] =
    new Zero[A] {
      def zero = ev.zero
    }

  implicit def sampleCanEq[A]: Eq[Sample[A]] =
    new Eq[Sample[A]] {
      def eqv(x: Sample[A], y: Sample[A]): Boolean = {
        x.rows == y.rows && x.cols == y.cols && x.depth == y.depth
      }
    }

  implicit def sampleIsMonoid[A: ClassTag]: Monoid[Sample[A]] =
    new Monoid[Sample[A]] {
      def combine(x: Sample[A], y: Sample[A]): Sample[A] =
        Sample(x.depth, x.rows, x.cols, x.number, x.data ++ y.data)

      def empty = Sample.empty[A](0, 0, 0)

    }

}
