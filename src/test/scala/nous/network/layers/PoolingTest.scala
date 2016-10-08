package nous.network.layers

import nous.network.definitions._
import org.scalatest.FunSuite
import spire.implicits._

class PoolingTest extends FunSuite {

  test("pool layer definition creation") {
    val poolDefinition = PoolDefinition[Double](maxpool = true, 2, 2)
    poolDefinition.build(Shape(1, 1, 28, 28))
  }

  test("pool layer creation") {
    val inputShape = Shape(1, 1, 28, 28)
    new Pooling[Double](PoolDefinition(maxpool = true, 2, 2), inputShape)
  }

  test("pool forward pass") {
    val testData = nous.StaticData.mnistTest(1).take(1).runLog.unsafeRun.head
    val inputShape = Shape(1, 1, 28, 28)
    val poolLayer = new Pooling[Float](PoolDefinition(maxpool = true, 2, 2), inputShape)
    val output = poolLayer.forward(testData)
  }

}