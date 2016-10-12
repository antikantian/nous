package nous.network
package layers

import scala.reflect.ClassTag

import fs2._
import nous.kernels._
import nous.network.activations._
import spire.algebra._

sealed trait LayerDefinition[A] {
  def activation: ActivationF[A]
  def initialization: WeightInit[A]
  def build(input: InputShape): FunctionLayer[A]
}

case class ConvDefinition[A: Blas: Field: ClassTag](
    filters        : Int,
    height         : Int,
    width          : Int,
    strideH        : Int,
    strideW        : Int,
    bias           : Boolean = true,
    initialization : WeightInit[A],
    activation     : ActivationF[A] = Linear(),
    lambda         : Option[LambdaInjection[A]] = None)
  extends LayerDefinition[A] { self =>

  def build(input: InputShape): Convolution[A] = {
    val numInputs = height * width * input.k
    val numWeights = numInputs * filters
    val initializedWeights = initialization.take(numWeights)
    new Convolution(self, input, initializedWeights.runLog.unsafeRun)
  }
}

case class FcDefinition[A: Field: Blas: ClassTag](
    outputs         : Int,
    bias            : Boolean = true,
    initialization  : WeightInit[A],
    activation      : ActivationF[A] = Linear(),
    lambda          : Option[LambdaInjection[A]] = None)
  extends LayerDefinition[A] { self =>

  def build(input: InputShape): FullyConnected[A] = {
    val numInputs = input.r * input.c * input.k
    val initializedWeights = initialization.take(numInputs * outputs)
    new FullyConnected[A](self, input, initializedWeights.runLog.unsafeRun)
  }
}

case class DropoutDefinition[A: Field: ClassTag](r: Double = 0.5) extends LayerDefinition[A] { self =>
  override def initialization = Stream.empty[Task, A]

  def build(input: InputShape): Dropout[A] = {
    new Dropout[A](self, input)
  }

  def activation = Linear[A]()
}

case class PoolDefinition[A: Field: Order: ClassTag](
    maxpool : Boolean,
    poolH   : Int,
    poolW   : Int,
    strideH : Option[Int] = None,
    strideW : Option[Int] = None)
  extends LayerDefinition[A] { self =>

  override def initialization = Stream.empty[Task, A]

  def activation = Linear[A]()

  def build(input: InputShape): Pooling[A] = {
    new Pooling[A](self, input)
  }
}




