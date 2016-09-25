package nous.util

import scala.{Vector => SVector}

import cats.{Applicative, Monoid}
import cats.data.{Xor, NonEmptyVector => Nev}
import nous.linalg.internal.{Container, ElementTag}
import nous.linalg.{Matrix, Vector}
import nous.util.exception._
import spire.algebra.Eq

class Sample[A](data: List[Matrix[A]])(private[nous] val num: Int = -1) { self =>
  val entries = data
  val number = num
  val rows = if (data.isEmpty) 0 else data.head.rows
  val cols = if (data.isEmpty) 0 else data.head.cols
  val depth = data.length
  val size = rows * cols * depth

  def head = entries.head

  def tail = entries.tail

  /** Map over each value in each entry in this sample */
  def map[B](f: A => B): Sample[B] =
    new Sample[B](self.entries.map(_.map(f)))(number)

  /** Map over each entry in this sample */
  def mapE[B](f: Matrix[A] => Matrix[B]): Sample[B] =
    new Sample(entries map f)(number)

  def renumber(n: Int) = new Sample[A](entries)(n)

}

object Sample extends SampleInstances {

  def apply[A](mat: Matrix[A]): Sample[A] =
    new Sample(List(mat))

  def apply[A](mat: Matrix[A]*): Sample[A] =
    new Sample(List(mat:_*))

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