package nous.data

import scala.reflect.ClassTag

import cats._
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import fs2._
import nous.data.Sample.samplesMatch
import nous.util.Shape
import nous.util.exception._

class Batch[X: ClassTag, Y: ClassTag](samples: Vector[Sample[X, Y]]) { self =>

  val data = samples

  val size = data.length

  val xChannels = if (data.isEmpty) 0 else samples.head.channels

  val xHeight = if (data.isEmpty) 0 else samples.head.height

  val xWidth = if (data.isEmpty) 0 else samples.head.width

  val shape = Shape(size, xChannels, xHeight, xWidth)

  val k = xChannels
  val r = xHeight
  val c = xWidth

  lazy val targetSize = samples.foldLeft(0)((total, sb) => total + sb.target.length)

  def ++(rhs: Batch[X, Y]): Batch[X, Y] =
    Semigroup[Batch[X, Y]].combine(self, rhs)

  def head = data.head

  def tail = new Batch(data.tail)

  def take(n: Int): Batch[X, Y] = new Batch(data.take(n))

  def foreach(f: Sample[X, Y] => Unit): Unit =
    data foreach f

  def foldLeft[B](b: B)(f: (B, Sample[X, Y]) => B): B =
    data.foldLeft(b)(f)

  def map[XX: ClassTag](f: Sample[X, Y] => Sample[XX, Y]): Batch[XX, Y] =
    new Batch(data.map(f))

  def sampleStream: Stream[Pure, Sample[X, Y]] = Stream.emits(samples).pure

  def xStream: Stream[Pure, Array[X]] =
    data.foldLeft(Stream.empty[Pure, Array[X]]) { (stream, sample) =>
      stream ++ Stream.emit(sample.data)
    }

  def yStream: Stream[Pure, Array[Y]] =
    data.foldLeft(Stream.empty[Pure, Array[Y]]) { (stream, sample) =>
      stream ++ Stream.emit(sample.target)
    }

  def getSample(n: Int): Option[Sample[X, Y]] =
    data.find(_.sample == n)

  def targets: Vector[Array[Y]] = data.map(_.target)

  def renumberAll(snums: Vector[Int]) = {
    val renumbered =
      data.zip(snums) map { sidx =>
        val (sample, snum) = sidx
        sample.renumber(snum)
      }
    new Batch(renumbered)
  }

  def renumberWith(nmap: Map[Int, Int]) = {
    val renumbered =
      sampleStream map { sample =>
        val snum = nmap.getOrElse(sample.sample, sample.sample)
        sample.renumber(snum)
      }
    new Batch(renumbered.toVector)
  }

  def arrayX: Array[X] =
    foldLeft(new Array[X](k * r * c)) { (acc, sample) => acc ++ sample.data }

  def arrayY: Array[Y] =
    foldLeft(new Array[Y](targetSize)) { (acc, sample) => acc ++ sample.target }

  def toArray: (Array[X], Array[Y]) = (arrayX, arrayY)

  def shuffle: Batch[X, Y] = new Batch(scala.util.Random.shuffle(samples))

  def zipWith(s: Seq[X])(f: (Sample[X, Y], X) => Sample[X, Y]): Batch[X, Y] = {
    new Batch(data.zip(s).map(f.tupled.apply(_)))
  }

}

object Batch extends BatchInstances {

  def validate[X, Y](bat: Batch[X, Y]): Validated[SamplesInconsistent, Batch[X, Y]] = {
    val shape = Seq(bat.xChannels, bat.xHeight, bat.xWidth)
    val mismatched = bat.data.tail collect {
      case sample if !samplesMatch(bat.data.head, sample) => sample.sample
    }
    if (mismatched.isEmpty)
      Valid(bat)
    else
      Invalid(SamplesInconsistent(mismatched, shape, s"""Samples[${mismatched.mkString(",")}] do not match shape: (${shape.mkString(", ")})"""))
  }

  def checkBatches[X: ClassTag, Y: ClassTag](b1: Batch[X, Y], b2: Batch[X, Y]) = {
    validate(b1).combine(validate(b2))
  }

}

sealed abstract class BatchInstances {
  implicit def batchSemigroup[X: ClassTag, Y: ClassTag]: Semigroup[Batch[X, Y]] =
    new Semigroup[Batch[X, Y]] {
      def combine(x: Batch[X, Y], y: Batch[X, Y]): Batch[X, Y] =
        new Batch(x.data ++ y.data)
    }
}