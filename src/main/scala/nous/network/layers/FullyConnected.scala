package nous.network
package layers

import scala.reflect.ClassTag

import nous.kernels._
import nous.network.definitions._
import spire.algebra._
import spire.math._

class FullyConnected[A: ClassTag](fcdef: FcDefinition[A], input: Shape, w: Vector[A])(
    implicit val field: Field[A], val linalg: Blas[A]) extends FunctionLayer[A] { self =>

  val definition = fcdef
  val activate = definition.activation.getOrElse(activations.linear[A] _)
  val weights = w
  val bias =
    if (definition.bias)
      Vector.fill(definition.outputs)(field.zero)
    else
      Vector.empty[A]

  val numOutputs = definition.outputs

  val inputShape = input
  val outputShape = Shape(input.s, 1, 1, definition.outputs)

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x map { sample =>
      //beta, dest_tensor, alpha, tensor_lhs, trans_lhs(bool), tensor_rhs, trans_rhs(bool)
      // Multiplying layer input x weights
      // a : sample
      // b : weights
      // m : rows of a == rows of c
      // n : cols of b == cols of c
      // k : cols of a == rows of b
      val a_m = sample.rows
      val a_n = sample.cols
      val b_m = a_n
      val b_n = numOutputs
      val c_m = a_m
      val c_n = b_n
      val m = c_m
      val n = b_n
      val k = a_n
      val y = linalg.gemm("N", "N", m, n, k, field.one, sample.data.toArray, weights.toArray, field.zero)
      if (bias.nonEmpty) {
        val yb = y.zip(bias).map(aa => field.plus(aa._1, aa._2))
        sample.update(1, 1, numOutputs, yb.toVector.map(activate))
      } else {
        sample.update(1, 1, numOutputs, y.toVector.map(activate))
      }
    }
  }

  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A] = {
    weights
  }

  def updateW(weights: Vector[A]): FullyConnected[A] = {
    self
  }

}