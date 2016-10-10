package nous.network.layers

import nous.network.activations.ReLU
import nous.network.definitions._
import nous.util._
import org.scalatest.FunSuite
import spire.implicits._

class FullyConnectedTest extends FunSuite {

  val init = Initializer.uniform[Float](-1, 1)
  val activationF = ReLU[Float]()
  val testData = nous.StaticData.mnistTrain(1).take(1).runLog.unsafeRun.head

  test("fc layer definition") {
    FcDefinition[Float](512, bias = true, init, activationF)
  }

  test("fc layer build from definition") {
    val inputShape = Shape(1, 1, 28, 28)
    val fcDefinition = FcDefinition[Float](512, bias = true, init, activationF)
    val fcLayer = fcDefinition.build(inputShape)
  }

  test("fc forward pass") {
    val inputShape = Shape(1, 1, 28, 28)
    val fcDefinition = FcDefinition[Float](1, bias = true, init, activationF)
    val fcLayer = fcDefinition.build(inputShape)
    val output = fcLayer.forward(testData)
    println(output.data.head.data)
  }

}