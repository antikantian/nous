package nous.network
package layers

import scala.reflect.ClassTag

import nous.data._
import nous.kernels._
import nous.network.definitions._
import nous.util.Shape
import spire.algebra._
import spire.math._

class FullyConnected[A: ClassTag](fcdef: FcDefinition[A], in: InputShape, w: Vector[A], id: Int = -1)(
    implicit val field: Field[A], val lin: Blas[A]) extends FunctionLayer[A] { self =>

  val layerID = id
  val layerLabel = "fc"
  val definition = fcdef
  val activate = definition.activation
  val numInputs = in.r * in.c * in.k
  val numOutputs = definition.outputs

  val W: Weights[A] = {
    val w_length = numInputs * numOutputs
    assert(w.length == w_length, s"Provided weights don't match expected length of $w_length")
    Weights(numInputs, numOutputs, 1, 1, w)
  }

  val bias =
    if (definition.bias)
      Vector.fill(definition.outputs)(field.zero)
    else
      Vector.empty[A]

  val inputShape = in
  val outputShape = Shape(inputShape.n, numOutputs, 1, 1)

  def renumber(n: Int): FullyConnected[A] = new FullyConnected[A](definition, inputShape, w, n)

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x map { sample =>
      val transA = "N"
      val transB = "T"
      val m = 1
      val n = numOutputs
      val k = sample.totalSize
      val alpha = field.one
      val beta = field.one
      val y = lin.gemm(transA, transB, m, n, k, alpha, sample.data.toArray, W.toArray, beta)
      if (bias.nonEmpty) {
        val yb = y.zip(bias).map(aa => field.plus(aa._1, aa._2))
        sample.update(numOutputs, 1, 1, yb.toVector)
      } else {
        sample.update(numOutputs, 1, 1, y.toVector)
      }
    }
  }

  def backward(x: LayerInput[A], gradient: Vector[A]): Vector[A] = {
    Vector.empty[A]
  }

  def updateW(weights: Vector[A]): FullyConnected[A] = {
    self
  }

}