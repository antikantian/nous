package nous.network
package layers

import scala.reflect.ClassTag

import nous.kernels.Blas
import nous.kernels.convolution._
import nous.network.definitions._
import spire.algebra._
import spire.math._

class Pooling[A: ClassTag](pdef: PoolDefinition[A], input: Shape)(
    implicit val field: Field[A], val order: Order[A]) extends FunctionLayer[A] { self =>

  val definition = pdef
  val weights = Vector.empty[A]
  val bias = weights

  val maxpool = definition.maxpool
  val windowH = definition.poolH
  val windowW = definition.poolW
  val strideH = definition.strideH.getOrElse(windowH)
  val strideW = definition.strideW.getOrElse(windowW)
  val paddingH = if (strideH != 0) 0 else windowH / 2
  val paddingW = if (strideW != 0) 0 else windowW / 2

  val inputN = input.s
  val inputC = input.k
  val inputH = input.m
  val inputW = input.n
  val inputShape = Shape(inputN, inputC, inputH, inputW)

  val outputN = inputN
  val outputC = inputC
  val outputH = (inputH - windowH) / strideH + 1
  val outputW = (inputW - windowW) / strideW + 1
  val outputShape = Shape(outputN, outputC, outputH, outputW)

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x map { sample =>
      val os = sample.mapChannels[A] { channel =>
        pool(channel, windowH, windowW, strideH, strideW, paddingH, paddingW, maxpool)
      }
      sample.update(os.channels, os.height, os.width, os.data)
    }
  }

  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A] = {
    weights
  }

  def updateW(weights: Vector[A]): Pooling[A] = {
    self
  }

}
