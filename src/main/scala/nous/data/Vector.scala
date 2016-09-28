package nous.data

import cats._
import spire.math._

/**

class Vector[A](_length: Int)(values: Array[A]) extends Matrix[A](1, _length) {

  val size = _length

  def head: A = data.head

  def tail: Vector[A] =
    new Vector[A](data.tail.length)(data.tail)

  override def map[B](f: A => B): Vector[B] =
    new Vector(size)(data map f)

  def flatMap[B](f: A => Vector[B]): Vector[B] =
    new Vector(size)(data.flatMap(a => f(a).toArray))

  def scanLeft[B](b: B)(f: (B, A) => B): Vector[B] =
    new Vector(size)(data.scanLeft(b)(f))

  def zipMap[B, C](v: Vector[B])(f: (A, B) => C): Vector[C] = {
    val dz = data.zip(v.data) map { ab =>
      val (a, b) = ab
      f(a, b)
    }
    new Vector(size)(dz)
  }

  def foreach(f: A => Unit): Unit =
    data foreach f

  def sorted: Vector[A] =
    new Vector(size)(Sorting.quickSort(data))

  def reversed: Vector[A] =
    new Vector(size)(data.reverse)

  def slice(from: Int, until: Int, stride: Int = 1): Vector[A] =
    if (stride > 1) {
      val sliced = data.slice(from, until).zipWithIndex map { ai =>
        val (a, i) = ai
        (a, i + 1)
      }
      val collected = sliced collect {
        case (a, i) if i % stride == 0 => a
      }
      new Vector[A](collected.length)(collected)
    } else {
      new Vector[A](until - from)(data.slice(from, until))
    }

  def toSeq = data.toSeq

}

object Vector {
  def apply[A](size: Int): Vector[A] = new Vector(size)(new Array[A](size))
  def zeros[A](size: Int)(implicit ev: Numeric[A]): Vector[A] = new Vector(size)(Array.fill(size)(ev.zero))
}

class ColumnVector[A](_length: Int) extends Vector[A](_length) {
  override val m = data.length
  override val n = 1
}

class RowVector[A](_length: Int) extends Vector[A](_length)

sealed abstract class VectorInstances {

  implicit val vectorIsApplicative: Applicative[Vector] = new Applicative[Vector] {
    def pure[A](a: A): Vector[A] = new Vector(1)(Array(a))
    def ap[A, B](ff: Vector[A => B])(fa: Vector[A]): Vector[B] =
      ff.flatMap(f => fa.map(f))
  }

}

*/