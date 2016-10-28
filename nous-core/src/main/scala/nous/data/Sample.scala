package nous.data

import scala.reflect.ClassTag
import scala.{Vector => SVector}

import fs2._
import spire.algebra.Eq

class Sample[X: ClassTag, Y](c: Int, h: Int, w: Int, x: Array[X], y: Array[Y], snum: Int = -1) { self =>
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

  def dataStream: Stream[Pure, X] = Stream.emits(data).pure

  def channelStream: Stream[Pure, Channel[X]] =
    dataStream.vectorChunkN(channelSize).map(v => Channel(height, width, v.toArray))

  def indexOf(nchan: Int, nrow: Int, ncol: Int): Int =
    ((depth + nchan) * rows + nrow) * cols + ncol

  def atPosition(d: Int, x: Int, y: Int): X =
    data.apply { ((depth + d) * rows + y) * cols + x }

  def map[XX: ClassTag](f: X => XX): Sample[XX, Y] =
    new Sample(channels, height, width, data.map(f), target, sample)

  def mapChannels[XX: ClassTag](f: Channel[X] => Channel[XX]): Sample[XX, Y] = {
    val newChannels = channelStream.map(f).toVector
    val outH = newChannels.head.height
    val outW = newChannels.head.width
    new Sample(channels, outH, outW, newChannels.foldLeft(Array.empty[XX])(_ ++ _.data), y, sample)
  }

  def renumber(n: Int) = new Sample[X, Y](channels, height, width, data, target, n)

  def boundary: Rectangle = Rectangle(0, 0, cols - 1, rows - 1)

  def update(c: Int, h: Int, w: Int, x: Array[X]): Sample[X, Y] =
    new Sample(c, h, w, x, target, sample)

  def update(x: Array[X]): Sample[X, Y] =
    new Sample(channels, height, width, x, target)

  override def toString =
    s"(Sample $sample) h: $height; w: $width; c: $channels; size: $totalSize"

}

object Sample {

  case class Columned[X: ClassTag, Y](
      h       : Int,
      w       : Int,
      kernel  : Int,
      stride  : Int,
      padding : Int,
      cols    : Array[X],
      origin  : Sample[X, Y]) {
    def revert = origin
  }

  def apply[X: ClassTag, Y](c: Int, h: Int, w: Int, x: Array[X], y: Array[Y], snum: Int): Sample[X, Y] =
    new Sample(c, h, w, x, y, snum)

  def samplesMatch[A, T](s1: Sample[A, T], s2: Sample[A, T]): Boolean =
    if (s1.channels == s2.channels && s1.rows == s2.rows && s1.cols == s2.cols) true else false

}

sealed abstract class SampleInstances {

  implicit def sampleCanEq[X, Y]: Eq[Sample[X, Y]] =
    new Eq[Sample[X, Y]] {
      def eqv(x: Sample[X, Y], y: Sample[X, Y]): Boolean = {
        x.rows == y.rows && x.cols == y.cols && x.depth == y.depth && x.data.length == y.data.length
      }
    }

}


