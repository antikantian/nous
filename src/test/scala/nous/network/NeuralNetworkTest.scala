package nous.network

import nous.kernels.activations._
import nous.util.Initializer
import nous.network.definitions._
import nous.network.layers._
import org.scalatest.FunSuite
import spire.implicits._

class NeuralNetworkTest extends FunSuite {

  val inputShape = Shape(1, 3, 5, 5)
  val init = Initializer.uniform[Double](-1, 1)
  val ld1 = ConvDefinition[Double](2, 3, 3, 2, 1, bias = true, init, relu[Double])
  val layerdefs = List(ld1)
  val testInput = nous.StaticData.batch1x3x5x5

  test("neural network creation") {
    NeuralNetwork.build[Double](layerdefs, inputShape)
  }

  test("neural network forward propagation") {
    val net = NeuralNetwork.build[Double](layerdefs, inputShape)
    println(s"${testInput.shape}")
    val output = net.propagate(testInput)
    println(s"${output.shape}")

    output foreach { sample =>
      println(sample.data)
    }
  }

}