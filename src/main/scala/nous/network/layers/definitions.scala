package nous.network.layers

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

import nous.util.Blas
import nous.util.exception._
import spire.algebra._
import spire.math._

object definitions {

  sealed trait LayerDefinition[A] {
    def build(input: InputShape)(implicit ev: Numeric[A]): HiddenLayer[A]
  }

  case class ConvDefinition[A: Blas: Field: ClassTag: TypeTag](
      filters        : Int,
      height         : Int,
      width          : Int,
      stride         : Int,
      padding        : Int,
      bias           : Boolean,
      initialization : WeightInit[A],
      activation     : Activation[A],
      lambda         : Option[LambdaInjection[A]] = None)
  extends LayerDefinition[A] { self =>

    def build(input: InputShape)(implicit ev: Numeric[A]): Convolution[A] =
      input match {
        case shape@Shape4D(s, c, h, w) =>
          val numInputs = height * width * c
          val numWeights = numInputs * filters
          val initializedWeights = initialization.take(numWeights)
          new Convolution(self, shape, initializedWeights.runLog.unsafeRun)
        case _ =>
          throw InvalidInputShape(s"This convolution layer expects an input shape of (s, c, h, w), but was given: $input")
      }
  }

  /**
  case class LambdaDefintion[A](
      output          : LayerShape,
      forward         : LayerInput[A] => LayerOutput[A],
      backward        : LayerOutput[A] => Vector[A],
      initialization  : WeightInit[A],
      activation      : Activation[A])
    extends LayerDefinition[A] {

    def build(input: InputShape)(implicit ev: Numeric[A]): Lambda[A] = {
      val initializedWeights = initialization.take
    }

  }
   */



}