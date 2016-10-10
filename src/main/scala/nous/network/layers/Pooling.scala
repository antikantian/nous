package nous.network
package layers

import scala.reflect.ClassTag

import nous.data._
import nous.kernels.convolution._
import nous.network.definitions._
import nous.util.Shape
import spire.algebra._
import spire.math._

class Pooling[A: ClassTag](pdef: PoolDefinition[A], in: InputShape, id: Int = -1)(
    implicit val field: Field[A], val order: Order[A]) extends FunctionLayer[A] { self =>

  val layerID = id
  val layerLabel = {
    val prefix = pdef.maxpool match {
      case true => "max"
      case false => "avg"
    }
    s"${prefix}pool"
  }

  val definition = pdef
  val maxpool = definition.maxpool
  val windowH = definition.poolH
  val windowW = definition.poolW
  val strideH = definition.strideH.getOrElse(windowH)
  val strideW = definition.strideW.getOrElse(windowW)
  val paddingH = if (strideH != 0) 0 else windowH / 2
  val paddingW = if (strideW != 0) 0 else windowW / 2

  val inputShape = in
  val outputN = inputShape.n
  val outputC = inputShape.k
  val outputH = (inputShape.r - windowH) / strideH + 1
  val outputW = (inputShape.c - windowW) / strideW + 1
  val outputShape = Shape(outputN, outputC, outputH, outputW)

  lazy val W = Weights.empty[A]
  lazy val bias = Vector.empty[A]

  def renumber(n: Int): Pooling[A] = new Pooling[A](definition, inputShape, n)

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x map { sample =>
      val os = sample.mapChannels[A] { channel =>
        pool(channel, windowH, windowW, strideH, strideW, paddingH, paddingW, maxpool)
      }
      sample.update(os.channels, os.height, os.width, os.data)
    }
  }

  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A] = {
    W.data
  }

  def updateW(weights: Vector[A]): Pooling[A] = {
    self
  }

}
