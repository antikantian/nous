package nous

import nous.data.Batch
import nous.datasets.mnist._
import nous.network.NeuralNetwork
import nous.network.activations.{ReLU, Softmax}
import nous.network.layers.FcDefinition
import nous.network.loss.CrossEntropy
import nous.util.Initializer
import spire.implicits._

object Test extends App {

  val init = Initializer.uniform[Float](-1, 1)

  val lossFunction = CrossEntropy[Float]()

  val testData = mnistTest(1).take(1).runLog.unsafeRun.head

  val layerdef1 = FcDefinition[Float](512, bias = false, init, ReLU[Float]())
  //val layer1do = DropoutDefinition[Float](0.2)
  val layerdef2 = FcDefinition[Float](512, bias = false, init, ReLU[Float]())
  //val layer2do = DropoutDefinition[Float](0.2)
  val layerdef3 = FcDefinition[Float](10, bias = false, init, Softmax[Float]())

  val net = NeuralNetwork.build[Float](List(layerdef1, layerdef2, layerdef3), testData.shape, lossFunction)
  val forward = net.propagate(testData)

  def mnistTest(batchSize: Int) =
    testingSamples.vectorChunkN(batchSize) map { samples =>
      new Batch[Float, Float](samples)
    }





}