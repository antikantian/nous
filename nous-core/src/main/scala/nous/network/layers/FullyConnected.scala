package nous.network
package layers

import scala.reflect.ClassTag

import cats.Eval
import nous.data._
import nous.kernels._
import nous.util.Shape
import spire.algebra._
import spire.implicits._

class FullyConnected[A: ClassTag](fcdef: FcDefinition[A], in: InputShape, w: Array[A], id: Int = -1)(
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
      Array.fill(definition.outputs)(field.zero)
    else
      Array.empty[A]

  val inputShape = in
  val outputShape = Shape(inputShape.n, numOutputs, 1, 1)

  def renumber(n: Int): FullyConnected[A] = new FullyConnected[A](definition, inputShape, w, n)

  def forward(x: Sample[A, A]): Sample[A, A] = {
    val transA = "N"
    val transB = "N"

    val input_rows_m = 1
    val input_cols_k = numInputs
    val w_rows_k = numInputs
    val w_cols_n = numOutputs

    val lda = input_rows_m
    val ldb = w_rows_k
    val ldc = input_rows_m

    val alpha = field.one
    val beta = field.zero

    val m = input_rows_m
    val n = w_cols_n
    val k = w_rows_k

    val y = lin.gemm(transA, transB, m, n, k, alpha, x.data, lda, W.toArray, ldb, beta, ldc)

    println(s"$layerName: inputs = $numInputs, outputs = $numOutputs, data = ${x.data.length}, weights = ${W.data.length}, output-actual = ${y.length}")

    val sampleOut =
      if (bias.nonEmpty) {
        val yb = y.zip(bias).map(aa => field.plus(aa._1, aa._2))
        x.update(numOutputs, 1, 1, yb)
      } else {
        x.update(numOutputs, 1, 1, y)
      }
    a.forward(sampleOut)
  }

  def backward(g: Array[A], xa: Array[A]): Array[A] = {
    println(s"""Gradient: $numOutputs x $numInputs; length: ${g.length}""")
    /**
    val b = W.toArray
    val m = numOutputs
    val n = numInputs
    val k = 1
    val alpha = field.one
    val beta = field.zero
    val gw = lin.gemm("N", "T", numOutputs, numInputs, 1, alpha, g, numOutputs, b, n, beta, m)
    //val gx = Eval.later(Array.fill(xa.length)(field.one) - xa).map(_.zip(xa).map(aa => aa._1 * aa._2))
    //gw.zip(gx.value).map(gxgw => gxgw._1 * gxgw._2)
    gw
     */
    xa
  }

  def updateW(weights: Array[A]): FullyConnected[A] = {
    self
  }

}