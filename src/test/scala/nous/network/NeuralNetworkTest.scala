package nous.network

import fs2._
import nous.util.Initializer
import nous.network.activations._
import nous.network.definitions._
import nous.network.loss._
import org.scalatest.FunSuite
import spire.implicits._

class NeuralNetworkTest extends FunSuite {

  val mnist = nous.StaticData.mnistTrain(1).take(1).runLog.unsafeRun.head
  val inputShape = mnist.shape
  val init = Initializer.uniform[Float](-1, 1)
  val ld1 = ConvDefinition[Float](32, 3, 3, 1, 1, bias = true, init, ReLU[Float]())
  val layerdefs = List(ld1)
  val lossFunction = MeanSquaredError[Float]()
  //val testInput = nous.StaticData.batch1x3x5x5

  /**
  test("neural network creation") {
    NeuralNetwork.build[Float](layerdefs, inputShape, lossFunction)
  }
   */

  /**
  test("neural network forward propagation") {
    val net = NeuralNetwork.build[Float](layerdefs, inputShape, lossFunction)
    val output = net.propagate(mnist)
  }
   */

  /**
  test("neural network cycle") {
    val layerdef1 = FcDefinition[Float](512, bias = true, init, ReLU[Float]())
    val layerdef2 = DropoutDefinition[Float](0.2)
    val layerdef3 = FcDefinition[Float](512, bias = true, init, ReLU[Float]())
    val layerdef4 = DropoutDefinition[Float](0.2)
    val layerdef5 = FcDefinition[Float](1, bias = true, init, ReLU[Float]())
    val layerdefs = List(layerdef1, layerdef2, layerdef3, layerdef4, layerdef5)
    val testInput = nous.StaticData.mnistTest(1).take(1).runLog.unsafeRun.head

    val net = NeuralNetwork.build[Float](layerdefs, testInput.shape, lossFunction)
    val output = net.propagate(testInput)
    val loss = net.lossF.forward(output)
    println(output.data.head.data)
    println(loss)
  }
   */

  test("neural network cycle") {
    val layerdef1 = FcDefinition[Float](1, bias = false, init, Sigmoid[Float]())
    val testInput = nous.StaticData.sampleBatch

    val net = NeuralNetwork.build[Float](List(layerdef1), testInput.shape, lossFunction)
    val output = net.propagate(testInput)
    val loss = net.lossF.f(output)
    output foreach { sample =>
      println(s"""Prediction: [${sample.data.mkString(", ")}], target: ${sample.target}""")
    }
    println(s"""Total error: ${loss.et}, (y - t): [${loss.eo.mkString(", ")}]""")

  }

}