package nous.data

import scala.collection.mutable
import scala.{Vector => SVector}
import scala.reflect.ClassTag

import breeze.linalg.DenseMatrix
import breeze.storage.Zero
import cats._
import cats.data._
import cats.data.Validated.{ Invalid, Valid }
import fs2._
import nous.kernels.convolution._
import nous.util.exception._
import spire.algebra.Eq
import spire.math._

class Sample[A: ClassTag: Numeric, T](c: Int, h: Int, w: Int, x: Vector[A], y: T, snum: Int = -1) { self =>

  import Sample._

  val data = x
  val channels = c
  val height = h
  val width = w
  val target = y
  val sample = snum

  val depth = channels
  val rows = height
  val cols = width
  val channelSize = rows * cols
  val totalSize = channelSize * depth

  def dataStream: Stream[Pure, A] = Stream.emits(data).pure
  def channelStream: Stream[Pure, Vector[A]] = dataStream.vectorChunkN(channelSize)

  def indexOf(nchan: Int, nrow: Int, ncol: Int): Int =
    ((depth + nchan) * rows + nrow) * cols + ncol

  def atPosition(nchan: Int, nrow: Int, ncol: Int): A =
    data.apply { ((depth + nchan) * rows + nrow) * cols + ncol }

  def map[B: ClassTag: Numeric](f: A => B): Sample[B, T] =
    new Sample(channels, height, width, dataStream.map(f).toVector, target, sample)

  def renumber(n: Int) = new Sample[A, T](channels, height, width, data, target, n)

  def boundary: Rectangle = Rectangle(0, 0, cols - 1, rows - 1)

  def toCol(kernelSize: Int, stride: Int, padding: Int) = {
    val colSize = kernelSize * kernelSize * depth
    val hw = (height - kernelSize) / stride + 1
    val hw2 = hw * hw
    val outputArray = im2col(data, channels, height, width, kernelSize, stride, padding)
    Columned(hw, hw, kernelSize, stride, padding, outputArray.toVector, self)
  }

  def update(nchan: Int, nh: Int, nw: Int, x: Vector[A]): Sample[A, T] =
    new Sample(nchan, nh, nw, x, target, sample)

  override def toString =
    s"(Sample $sample) h: $height; w: $width; c: $channels; size: $totalSize"

}

object Sample {

  case class Columned[A: ClassTag: Numeric, B](
      h: Int,
      w: Int,
      kernel: Int,
      stride: Int,
      padding: Int,
      cols: Vector[A],
      origin: Sample[A, B]) {
    def revert = origin
  }

  def apply[A: ClassTag: Numeric, T](c: Int, h: Int, w: Int, x: Vector[A], y: T, snum: Int): Sample[A, T] =
    new Sample(c, h, w, x, y, snum)

  def samplesMatch[A, T](s1: Sample[A, T], s2: Sample[A, T]): Boolean =
    if (s1.channels == s2.channels && s1.rows == s2.rows && s1.cols == s2.cols) true else false

}

/**
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
 */

