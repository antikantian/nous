package nous.datasets

import java.io.InputStream
import java.util.zip.GZIPInputStream

import fs2._
import fs2.io._
import nous.data._

object mnist {

  val trainingImages = Task.delay {
    new GZIPInputStream(getClass.getResourceAsStream("/mnist/train-images-idx3-ubyte.gz"))
  }

  val trainingLabels = Task.delay {
    new GZIPInputStream(getClass.getResourceAsStream("/mnist/train-labels-idx1-ubyte.gz"))
  }

  val testingImages = Task.delay {
    new GZIPInputStream(getClass.getResourceAsStream("/mnist/t10k-images-idx3-ubyte.gz"))
  }

  val testingLabels = Task.delay {
    new GZIPInputStream(getClass.getResourceAsStream("/mnist/t10k-labels-idx1-ubyte.gz"))
  }

  def buildStream(x: Task[InputStream], y: Task[InputStream]): Stream[Task, Sample[Float, Float]] =
    readInputStream(x, 4096)
      .bufferAll
      .drop(16)
      .map(byte => math.abs(byte.toFloat))
      .vectorChunkN(28 * 28)
      .zip(readInputStream(y, 4096).bufferAll.drop(8).map(_.toFloat))
      .zipWithIndex
      .map(xyi => new Sample(1, 28, 28, xyi._1._1.toArray, Array(xyi._1._2), xyi._2))

  def trainingSamples: Stream[Task, Sample[Float, Float]] = buildStream(trainingImages, trainingLabels)

  def testingSamples: Stream[Task, Sample[Float, Float]] = buildStream(testingImages, testingLabels)

}
