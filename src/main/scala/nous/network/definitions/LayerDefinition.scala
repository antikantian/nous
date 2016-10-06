package nous.network.definitions

import scala.reflect.ClassTag

import nous.kernels.activations._
import nous.kernels.Blas
import nous.network.layers._
import nous.util.exception.InvalidInputShape
import spire.algebra.Field
import spire.math.Numeric

sealed trait LayerDefinition[A] {
  def activation: Option[Activation[A]]
  def activationF = activation.getOrElse(linear)
  def initialization: WeightInit[A]
  def build(input: InputShape)(implicit ev: Numeric[A]): HiddenLayer[A]
}

case class ConvDefinition[A: Blas: Field: ClassTag](
    filters        : Int,
    height         : Int,
    width          : Int,
    stride         : Int,
    padding        : Int,
    bias           : Boolean,
    initialization : WeightInit[A],
    activation     : Option[Activation[A]] = None,
    lambda         : Option[LambdaInjection[A]] = None)
  extends LayerDefinition[A] { self =>

  def build(input: InputShape)(implicit ev: Numeric[A]): Convolution[A] =
    input match {
      case shape@Shape(s, k, m, n) =>
        val numInputs = height * width * k
        val numWeights = numInputs * filters
        val initializedWeights = initialization.take(numWeights)
        new Convolution(self, shape, initializedWeights.runLog.unsafeRun)
      case _ =>
        throw InvalidInputShape(s"This convolution layer expects an input shape of (s, c, h, w), but was given: $input")
    }
}

/**
case class MaxPoolDefinition[A: Blas: Field: ClassTag](
    poolH   : Int,
    poolW   : Int,
    stride  : Int,
    padding : Int)
  extends LayerDefinition[A] { self =>

  def build(input: InputShape)(implicit ev: Numeric[A]) = {

  }

}
 */

