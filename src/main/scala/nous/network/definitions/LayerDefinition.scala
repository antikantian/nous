package nous.network.definitions

import scala.reflect.ClassTag

import fs2._
import nous.kernels.activations._
import nous.kernels.Blas
import nous.network._
import nous.network.layers._
import nous.util.exception.InvalidInputShape
import spire.algebra._
import spire.math._

sealed trait LayerDefinition[A] {
  def activation: Option[ActivationF[A]]
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
    activation     : Option[ActivationF[A]] = None,
    lambda         : Option[LambdaInjection[A]] = None)
  extends LayerDefinition[A] { self =>

  def build(input: InputShape): Convolution[A] = {
    require(input.is4d, s"This convolution layer expects an input shape of (s, c, h, w), but was given: $input")
    val numInputs = height * width * input.k
    val numWeights = numInputs * filters
    val initializedWeights = initialization.take(numWeights)
    new Convolution(self, input, initializedWeights.runLog.unsafeRun)
  }
}

case class FcDefinition[A: Blas: Field: ClassTag](
    outputs         : Int,
    bias            : Boolean = true,
    initialization  : WeightInit[A],
    activation      : Option[ActivationF[A]] = None,
    lambda          : Option[LambdaInjection[A]] = None)
  extends LayerDefinition[A] { self =>

  def build(input: InputShape): FullyConnected[A] = {
    val numInputs = input.m * input.n * input.k
    val initializedWeights = initialization.take(numInputs)
    new FullyConnected[A](self, input, initializedWeights.runLog.unsafeRun)
  }
}

case class DropoutDefinition[A: Field: ClassTag](r: Double = 0.5) extends LayerDefinition[A] { self =>
  override def initialization = Stream.empty[Task, A]

  def build(input: InputShape): Dropout[A] = {
    new Dropout[A](self, input)
  }

  def activation = None
}

case class PoolDefinition[A: Field: Order: ClassTag](
    maxpool : Boolean,
    poolH   : Int,
    poolW   : Int,
    strideH : Option[Int] = None,
    strideW : Option[Int] = None)
  extends LayerDefinition[A] { self =>

  override def initialization = Stream.empty[Task, A]

  def activation = None

  def build(input: InputShape): Pooling[A] = {
    new Pooling[A](self, input)
  }
}

case class SoftmaxDefinition[A: Field: Trig: Order: NumberTag: ClassTag](
    implicit vs: NormedVectorSpace[Vector[A], A])
  extends LayerDefinition[A] { self =>

  override def initialization = Stream.empty[Task, A]

  def activation = None

  def build(input: InputShape) = {
    new Softmax[A](self, input)
  }
}



