package nous.util

import cats._

object exception {

  sealed abstract class NousException(msg: String) extends Exception(msg)

  sealed abstract class BundleException(msg: String) extends NousException(msg)
  case class BundleSamplesMismatch(msg: String) extends BundleException(msg)

  sealed abstract class SampleException(msg: String) extends NousException(msg)
  case class SampleMismatch(msg: String) extends SampleException(msg)
  case class SamplesInconsistent(num: Seq[Int], shape: Seq[Int], msg: String) extends SampleException(msg)

  sealed abstract class LayerException(msg: String) extends NousException(msg)
  case class ConvolutionOutputError(msg: String) extends LayerException(msg)

  sealed abstract class TensorException(msg: String) extends NousException(msg)
  case class TensorConcatError(msg: String) extends TensorException(msg)

  sealed abstract class BatchException(msg: String) extends NousException(msg)
  case class BatchesInconsistent(msg: String) extends BatchException(msg)

  sealed abstract class LayerDefinitionException(msg: String) extends NousException(msg)
  case class InvalidInputShape(msg: String) extends LayerDefinitionException(msg)

  implicit val samplesInconsistentSemigroup: Semigroup[SamplesInconsistent] =
    new Semigroup[SamplesInconsistent] {
      def combine(x: SamplesInconsistent, y: SamplesInconsistent): SamplesInconsistent = {
        val samples = x.num ++ y.num
        SamplesInconsistent(samples, x.shape, s"""Samples[${samples.mkString(", ")} do not match shape (${x.shape.mkString(", ")})""")
      }
    }

}
