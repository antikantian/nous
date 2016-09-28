package nous.data

import scala.annotation.tailrec
import scala.{Vector => SVector}

import cats._
import cats.data.{NonEmptyVector => Nev}
import fs2._
import spire.algebra.Eq
import spire.random._

/**

/** A bundle encapsulates data that's contained in a `Sample`. */
class Bundle[A](val samples: List[Sample[A]], val batch: Option[Int]) {
  val batchSize = batch.getOrElse(samples.length)
  val nSamples = samples.length
  val sampleR = samples.head.rows
  val sampleC = samples.head.cols
  val sampleD = samples.head.depth

  val totalSize = sampleR * sampleC * sampleD * nSamples

  def get(i: Int): Sample[A] = samples(i)

  def map[B](f: A => B): Bundle[B] =
    new Bundle(samples.map(s => s map f), batch)

  def streamA = {
    val as =
      for {
        s <- samples
        e <- s.entries
        m <- e.data
      } yield m
    Stream.emits[Pure, A](as)
  }

  def streamSamples = Stream.emits[Pure, Sample[A]](samples)

  def streamBatch = Stream.emits[Pure, Sample[A]](samples).sliding(batchSize)

  /** Generates an infinite stream of uniformly distributed random samples. */
  def infStream = {
    val dist = Uniform(0, nSamples)
    Stream.repeatEval(Task.delay(dist.apply(Generator.rng))) map { i => samples.apply(i) }
  }

  def dimsMatch(implicit ev: Eq[Sample[A]]): Boolean = {
    val sf = samples.tail.foldLeft((samples.head, false)) { (shb, sb) =>
      val (sa, _) = shb
      if (ev.eqv(sa, sb))
        (sb, true)
      else
        (sb, false)
    }
    sf._2
  }

}

object Bundle {

  def validateDims[A](v: Vector[Sample[A]])(implicit ev: Eq[Sample[A]]): Vector[Sample[A]] = {
    @tailrec
    def loop(sa: Sample[A], rest: Vector[Sample[A]], acc: Vector[Sample[A]]): Vector[Sample[A]] = {
      if (rest.tail.isEmpty)
        acc ++ Vector(sa, Sample.dimsMatchThrow(sa, rest.head))
      else
        loop(rest.head, rest.tail, acc ++ Vector(sa, Sample.dimsMatchThrow(sa, rest.head)))
    }
    if (v.tail.nonEmpty)
      loop(v.head, v.tail, Vector.empty[Sample[A]])
    else
      v // Only one sample, A = A
  }

}

sealed abstract class BundleInstances {

  implicit def bundleIsMonoid[A]: Monoid[Bundle[A]] = new Monoid[Bundle[A]] {
    def combine(b1: Bundle[A], b2: Bundle[A]) = {
      val sb1 = b1.samples
      val sb2 = b2.samples
      val sb3 = (sb1 ++ sb2).zipWithIndex map { (sidx) =>
        val (s, idx) = sidx
        s.renumber(idx)
      }
      new Bundle(sb3, b1.batch)
    }

    def empty = new Bundle[A](List.empty[Sample[A]], None)

  }

}

 */
