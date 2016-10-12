package nous.network
package layers

import scala.language.higherKinds
import scala.reflect.ClassTag

import nous.data._
import nous.kernels.Blas
import nous.kernels.convolution._
import nous.util.Shape
import nous.util.exception._
import spire.algebra._

class Convolution[A: ClassTag](cdef: ConvDefinition[A], in: InputShape, w: Vector[A], id: Int = -1)(
    implicit val field: Field[A], val linalg: Blas[A]) extends FunctionLayer[A] { self =>

  val layerID = id
  val layerLabel = "conv"
  val definition = cdef
  val activate = cdef.activation
  val filters = definition.filters
  val filterSize = definition.height
  val height = definition.height
  val width = definition.width
  val strideH = definition.strideH
  val strideW = definition.strideW
  val paddingH = if (strideH != 0) 0 else height / 2
  val paddingW = if (strideW != 0) 0 else width / 2

  val W = {
    val w_length = (in.r * in.c * in.k) * filters
    assert(w.length == w_length, s"Provided weights don't match expected length of $w_length")
    Weights(filters, in.k, height, width, w)
  }

  val bias =
    if (definition.bias)
      Vector.fill(definition.filters)(field.zero)
    else
      Vector.empty[A]

  val inputShape = in
  val outputN = inputShape.n
  val outputC = filters
  val outputH = Convolution.getOutputSize(inputShape.r, filterSize, strideH, paddingH)
  val outputW = Convolution.getOutputSize(inputShape.c, filterSize, strideW, paddingW)
  val outputShape = Shape(outputN, outputC, outputH, outputW)

  def renumber(n: Int): Convolution[A] = new Convolution[A](definition, inputShape, w, n)

  def forward(x: Sample[A, A]): Sample[A, A] = {
    val col = im2col(x.data, x.channels, x.height, x.width, filterSize, strideH, strideW, paddingH, paddingW)
    // Be wary of the transpose; we're storing row-major, BLAS is column-major
    // a : im2col(input_image)
    // b : weights
    // m : rows of a == rows of c
    // n : cols of b == cols of c
    // k : cols of a == rows of b
    val a_m = filterSize * filterSize * x.depth
    val a_n = outputH * outputW
    val b_m = filters
    val b_n = a_m
    val c_m = b_m
    val c_n = a_n
    val m = a_m
    val n = b_n
    val k = a_n
    val y = linalg.gemm("N", "T", m, n, k, field.one, col, W.toArray, field.one)
    if (bias.nonEmpty) {
      val yb =
        y.grouped(outputH * outputW)
          .toArray
          .zip(bias)
          .flatMap { outcb =>
            val (outputChannel, bias) = outcb
            outputChannel.map(element => field.plus(element, bias))
          }
      x.update(outputC, outputH, outputW, yb.toVector)
    } else {
      x.update(outputC, outputH, outputW, y.toVector)
    }
  }

  def backward(y: Vector[A], gradient: Vector[A]): Vector[A] = {
    W.data
  }

  def updateW(weights: Vector[A]): Convolution[A] = {
    self
  }

}

object Convolution {

  def getOutputSize(inputSize: Int, kernel: Int, stride: Int, padding: Int): Int =
    (inputSize + 2 * padding - kernel) / stride + 1

  def calcOutputVolume(inputSize: Int, kernelSize: Int, stride: Int, padding: Int, throwOnErr: Boolean = true): Int = {
    val os = (inputSize - kernelSize + 2d * padding) / stride + 1d

    if (throwOnErr && os % 1 != 0) {
      val message =
        s"""
           | Invalid output size of $os, based on:
           |  input_size($inputSize),
           |  kernel_size($kernelSize),
           |  padding_amount($padding).
         """.stripMargin
      throw ConvolutionOutputError(message)
    } else os.toInt
  }

}