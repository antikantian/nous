package nous.network

import nous.kernels.activations._
import nous.kernels.loss
import nous.util.Initializer
import nous.network.definitions._
import nous.network.layers._
import org.scalatest.FunSuite
import spire.implicits._

class NeuralNetworkTest extends FunSuite {

  val mnist = nous.StaticData.mnistTrain(1).take(1).runLog.unsafeRun.head
  val inputShape = mnist.shape
  val init = Initializer.uniform[Float](-1, 1)
  val ld1 = ConvDefinition[Float](32, 3, 3, 1, 1, bias = true, init, Some(relu[Float]))
  val layerdefs = List(ld1)
  val lossFunction = loss.mse[Float] _
  val testInput = nous.StaticData.batch1x3x5x5

  test("neural network creation") {
    NeuralNetwork.build[Float](layerdefs, inputShape, lossFunction)
  }

  test("neural network forward propagation") {
    val net = NeuralNetwork.build[Float](layerdefs, inputShape, lossFunction)
    println(mnist.data.head.data)
    println(mnist.data.head.target)
    val output = net.propagate(mnist)



    /**
    output foreach { sample =>
      println(sample.data)
    }
     */
  }

}