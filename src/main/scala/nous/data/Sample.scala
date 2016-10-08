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

class Sample[A: ClassTag, T](c: Int, h: Int, w: Int, x: Vector[A], y: T, snum: Int = -1) { self =>
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

  def channelStream: Stream[Pure, Channel[A]] =
    dataStream.vectorChunkN(channelSize).map(v => Channel(height, width, v))

  def indexOf(nchan: Int, nrow: Int, ncol: Int): Int =
    ((depth + nchan) * rows + nrow) * cols + ncol

  def atPosition(d: Int, x: Int, y: Int): A =
    data.apply { ((depth + d) * rows + y) * cols + x }

  def map[B: ClassTag](f: A => B): Sample[B, T] =
    new Sample(channels, height, width, dataStream.map(f).toVector, target, sample)

  def mapChannels[B: ClassTag](f: Channel[A] => Channel[B]): Sample[B, T] = {
    val newChannels = channelStream.map(f).toVector
    val outH = newChannels.head.height
    val outW = newChannels.head.width
    new Sample(channels, outH, outW, newChannels.foldLeft(Vector.empty[B])(_ ++ _.data), y, sample)
  }

  def renumber(n: Int) = new Sample[A, T](channels, height, width, data, target, n)

  def boundary: Rectangle = Rectangle(0, 0, cols - 1, rows - 1)

  def update(c: Int, h: Int, w: Int, x: Vector[A]): Sample[A, T] =
    new Sample(c, h, w, x, target, sample)

  def update(x: Vector[A]): Sample[A, T] =
    new Sample(channels, height, width, x, target)

  override def toString =
    s"(Sample $sample) h: $height; w: $width; c: $channels; size: $totalSize"

}

object Sample {

  case class Columned[A: ClassTag, B](
      h: Int,
      w: Int,
      kernel: Int,
      stride: Int,
      padding: Int,
      cols: Vector[A],
      origin: Sample[A, B]) {
    def revert = origin
  }

  def apply[A: ClassTag, T](c: Int, h: Int, w: Int, x: Vector[A], y: T, snum: Int): Sample[A, T] =
    new Sample(c, h, w, x, y, snum)

  def samplesMatch[A, T](s1: Sample[A, T], s2: Sample[A, T]): Boolean =
    if (s1.channels == s2.channels && s1.rows == s2.rows && s1.cols == s2.cols) true else false

}

sealed abstract class SampleInstances {

  implicit def sampleCanEq[A, T]: Eq[Sample[A, T]] =
    new Eq[Sample[A, T]] {
      def eqv(x: Sample[A, T], y: Sample[A, T]): Boolean = {
        x.rows == y.rows && x.cols == y.cols && x.depth == y.depth && x.data.length == y.data.length
      }
    }

}


