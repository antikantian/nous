package nous.network.layers

import fs2._
import nous.network.activations._
import nous.network.definitions._
import nous.util._
import org.scalameter._
import org.scalatest.FunSuite
import spire.implicits._
import spire.math.{ConvertableTo, Rational, Numeric}
import spire.algebra._

class ConvolutionTest extends FunSuite {

  val activationF = ReLU[Float]()
  val testData = nous.StaticData.mnistTrain(1).take(1).runLog.unsafeRun.head

  test("convolutional layer definition creation") {
    ConvDefinition[Float](32, 3, 3, 2, 2, bias = true, Initializer.uniform[Float](-1, 1), activationF)
  }

  test("convolutional layer creation") {
    val cdef = ConvDefinition[Float](32, 3, 3, 2, 2, bias = true, Initializer.uniform[Float](-1, 1), activationF)
    val inputShape = Shape(1, 1, 28, 28)
    val convLayer = cdef.build(inputShape)
  }

  test("convolution forward pass") {
    val cdef = ConvDefinition[Float](32, 3, 3, 1, 1, bias = true, Initializer.uniform[Float](-1, 1), activationF)
    val inputShape = Shape(1, 1, 28, 28)
    val convLayer = cdef.build(inputShape)
    val output = convLayer.forward(testData)
  }

}