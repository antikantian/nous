package nous.network.layers

import nous.network.definitions._
import org.scalatest.FunSuite
import spire.implicits._

class DropoutTest extends FunSuite {

  test("dropout layer definition creation") {
    val dropoutDefinition = DropoutDefinition[Double](0.5)
    dropoutDefinition.build(Shape(1, 3, 5, 5))
  }

  test("dropout layer creation") {
    val inputShape = Shape(1, 3, 5, 5)
    new Dropout[Double](DropoutDefinition(0.5), inputShape)
  }

  test("dropout forward pass") {
    val testData = nous.StaticData.mnistTrain(1).take(1).runLog.unsafeRun.head
    val inputShape = Shape(1, 1, 28, 28)
    val dropoutLayer = new Dropout[Float](DropoutDefinition(0.9), inputShape)
    dropoutLayer.forward(testData)
  }

}