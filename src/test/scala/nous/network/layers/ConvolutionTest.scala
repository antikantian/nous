package nous.network.layers

import fs2._
import nous.kernels.activations._
import nous.network.layers.definitions.ConvDefinition
import nous.util.Initializer
import org.scalameter._
import org.scalatest.FunSuite
import spire.implicits._
import spire.math.{ConvertableTo, Rational, Numeric}
import spire.random._

class ConvolutionTest extends FunSuite {

  test("convolutional layer definition creation") {
    ConvDefinition[Double](2, 3, 3, 2, 1, bias = true, Initializer.uniform[Double](-1, 1), sigmoid)
  }

  test("convolutional layer creation") {
    val cdef = ConvDefinition[Double](2, 3, 3, 2, 1, bias = true, Initializer.uniform[Double](-1, 1), sigmoid)
    val inputShape = Shape4D(1, 3, 5, 5)
    val staticWeights = nous.StaticData.filter3x3x3
    new Convolution[Double](cdef, inputShape, staticWeights)
  }

  test("convolution forward pass with double") {
    val cdef = ConvDefinition[Double](2, 3, 3, 2, 1, bias = true, Initializer.uniform[Double](-1, 1), sigmoid)
    val inputShape = Shape4D(1, 3, 5, 5)
    val staticWeights = nous.StaticData.filter3x3x3
    val convlayer = new Convolution[Double](cdef, inputShape, staticWeights)
    val batchInput = nous.StaticData.batch1x3x5x5
    val output = convlayer.forward(batchInput)
  }

}