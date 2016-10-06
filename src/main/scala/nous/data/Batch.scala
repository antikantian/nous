package nous.data

import scala.reflect.ClassTag

import cats._
import cats.data._
import cats.data.Validated.{ Invalid, Valid }
import fs2._
import nous.data.Sample.samplesMatch
import nous.network.layers._
import nous.util.exception._
import spire.math._

class Batch[A, T](samples: Vector[Sample[A, T]]) { self =>

  val data = samples

  val size = data.length

  val xChannels = if (data.isEmpty) 0 else samples.head.channels

  val xHeight = if (data.isEmpty) 0 else samples.head.height

  val xWidth = if (data.isEmpty) 0 else samples.head.width

  val shape = Shape(size, xChannels, xHeight, xWidth)

  def ++(rhs: Batch[A, T]): Batch[A, T] =
    Semigroup[Batch[A, T]].combine(self, rhs)

  def foreach(f: Sample[A, T] => Unit): Unit =
    data foreach f

  def map[B: ClassTag: Numeric](f: Sample[A, T] => Sample[B, T]): Batch[B, T] =
    new Batch(data.map(f))

  def sampleStream: Stream[Pure, Sample[A, T]] = Stream.emits(samples).pure

  def xStream: Stream[Pure, Vector[A]] =
    data.foldLeft(Stream.empty[Pure, Vector[A]]) { (stream, sample) =>
      stream ++ Stream.emit(sample.data)
    }

  def yStream: Stream[Pure, T] =
    data.foldLeft(Stream.empty[Pure, T]) { (stream, sample) =>
      stream ++ Stream.emit(sample.target)
    }

  def getSample(n: Int): Option[Sample[A, T]] =
    data.find(_.sample == n)

  def targets: Vector[T] = data.map(_.target)

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

}

object Batch extends BatchInstances {

  def validate[A, T](bat: Batch[A, T]): Validated[SamplesInconsistent, Batch[A, T]] = {
    val shape = Seq(bat.xChannels, bat.xHeight, bat.xWidth)
    val mismatched = bat.data.tail collect {
      case sample if !samplesMatch(bat.data.head, sample) => sample.sample
    }
    if (mismatched.isEmpty)
      Valid(bat)
    else
      Invalid(SamplesInconsistent(mismatched, shape, s"""Samples[${mismatched.mkString(",")}] do not match shape: (${shape.mkString(", ")})"""))
  }

  def checkBatches[A, T](b1: Batch[A, T], b2: Batch[A, T]) = {
    validate(b1).combine(validate(b2))
  }

}

sealed abstract class BatchInstances {
  implicit def batchSemigroup[A, T]: Semigroup[Batch[A, T]] =
    new Semigroup[Batch[A, T]] {
      def combine(x: Batch[A, T], y: Batch[A, T]): Batch[A, T] =
        new Batch(x.data ++ y.data)
    }
}