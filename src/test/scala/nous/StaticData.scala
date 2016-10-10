package nous

import nous.data.{ Batch, Sample }
import nous.datasets.mnist.{ trainingSamples, testingSamples }

object StaticData {

  val sample1 = Sample[Float, Float](1, 1, 3, Vector(0f, 0f, 1f), Vector(0f), 1)
  val sample2 = Sample[Float, Float](1, 1, 3, Vector(1f, 1f, 1f), Vector(1f), 2)
  val sample3 = Sample[Float, Float](1, 1, 3, Vector(1f, 0f, 1f), Vector(1f), 3)
  val sample4 = Sample[Float, Float](1, 1, 3, Vector(0f, 1f, 1f), Vector(0f), 4)
  val sampleBatch = new Batch(Vector(sample1, sample2, sample3, sample4))

  val input5x5x3 =
    Vector[Double](
      2, 1, 2, 0, 2,
      2, 1, 1, 2, 0,
      2, 0, 1, 2, 1,
      0, 0, 0, 2, 0,
      0, 2, 1, 1, 2,
      2, 0, 2, 0, 0,
      1, 2, 0, 0, 0,
      1, 1, 2, 2, 2,
      1, 2, 0, 2, 2,
      0, 2, 1, 1, 0,
      0, 2, 2, 2, 1,
      0, 0, 0, 2, 1,
      2, 0, 1, 0, 0,
      0, 1, 2, 2, 0,
      2, 1, 2, 1, 1)

  val w03x3x3 =
    Vector[Double](-1, 1, -1, 1, 1, 1, 1, 1, 0, 0, 1, -1, -1, -1, 1, -1, -1, 1, 1, -1, 0, 0, 0, -1, 0, 0, -1)

  val w13x3x3 =
    Vector[Double](-1, 0, -1, -1, 0, 0, 0, -1, -1, 1, 0, -1, -1, 0, 1, -1, -1, 1, 0, 1, 1, -1, 0, 1, 0, -1, 1)

  val filter3x3x3 = w03x3x3 ++ w13x3x3

  val bias1x1x2 = Vector[Double](1, 0)

  val sample3x5x5 = new Sample(3, 5, 5, input5x5x3, Vector(999d))

  val batch1x3x5x5 = new Batch(Vector(sample3x5x5))

  def mnistTrain(batchSize: Int) =
    trainingSamples.vectorChunkN(batchSize) map { samples =>
      new Batch[Float, Float](samples)
    }

  def mnistTest(batchSize: Int) =
    testingSamples.vectorChunkN(batchSize) map { samples =>
      new Batch[Float, Float](samples)
    }

}