package nous.network
package layers

import scala.reflect.ClassTag

import cats.Eval
import nous.data._
import nous.kernels._
import nous.util.Shape
import spire.algebra._
import spire.implicits._

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

  def forward(x: Sample[A, A]): Sample[A, A] = {
    val transA = "N"
    val transB = "T"
    //val m = 1
    //val n = numOutputs
    //val k = x.totalSize

    val m = 1
    val n = numOutputs
    val k = numInputs

    val alpha = field.one
    val beta = field.zero
    val lda = k
    val ldb = k
    val ldc = n
    //val y = lin.gemm(transA, transB, m, n, k, alpha, x.data.toArray, W.toArray, beta)
    val y = lin.gemm(transA, transB, m, n, k, alpha, x.data.toArray, lda, W.toArray, ldb, beta, ldc)

    println(s"$layerName: inputs = $numInputs, outputs = $numOutputs")
    println(s"$layerName: data = ${x.data.length}, weights = ${W.data.length}, output = ${y.length}")

    val sampleOut =
      if (bias.nonEmpty) {
        val yb = y.zip(bias).map(aa => field.plus(aa._1, aa._2))
        x.update(numOutputs, 1, 1, yb.toVector)
      } else {
        x.update(numOutputs, 1, 1, y.toVector)
      }
    a.forward(sampleOut)
  }

  def backward(gradient: Vector[A], xa: Vector[A]): Vector[A] = {
    val x = xa.toArray
    val weights = W.toArray
    val m = inputShape.r
    val n = numInputs
    val k = inputShape.r
    val alpha = field.one
    val beta = field.zero
    val gw = lin.gemm("T", "N", m, n, k, alpha, weights, x, beta)
    val gx = Eval.later(Array.fill(x.length)(field.one) - x).map(_.zip(x).map(aa => aa._1 * aa._2))
    gw.zip(gx.value).map(gxgw => gxgw._1 * gxgw._2).toVector
  }

  def updateW(weights: Vector[A]): FullyConnected[A] = {
    self
  }

}