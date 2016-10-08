package nous.network.layers

import scala.language.higherKinds
import scala.reflect.ClassTag

import nous.kernels.Blas
import nous.kernels.activations
import nous.kernels.convolution._
import nous.network.definitions._
import nous.util.exception._
import spire.algebra._
import spire.math._

class Convolution[A: ClassTag](cdef: ConvDefinition[A], input: Shape, w: Vector[A])(
    implicit val field: Field[A], val linalg: Blas[A]) extends FunctionLayer[A] { self =>

  val definition = cdef
  val activate = cdef.activation.getOrElse(activations.linear[A] _)
  val weights = w
  val bias =
    if (cdef.bias)
      Vector.fill(definition.filters)(field.zero)
    else
      Vector.empty[A]

  val filters = definition.filters
  val filterSize = definition.height
  val height = definition.height
  val width = definition.width
  val strideH = definition.strideH
  val strideW = definition.strideW
  val paddingH = if (strideH != 0) 0 else height / 2
  val paddingW = if (strideW != 0) 0 else width / 2

  val numBiases = bias.length
  val numWeights = weights.length
  val numParameters = numWeights + numBiases

  val inputN = input.s
  val inputC = input.k
  val inputH = input.m
  val inputW = input.n
  val inputShape = Shape(inputN, inputC, inputH, inputW)

  val outputN = input.s
  val outputC = filters
  val outputH = Convolution.getOutputSize(inputH, filterSize, strideH, paddingH)
  val outputW = Convolution.getOutputSize(inputH, filterSize, strideW, paddingW)
  val outputShape = Shape(outputN, outputC, outputH, outputW)

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x map { sample =>
      val col = im2col(sample.data, sample.channels, sample.height, sample.width, filterSize, strideH, strideW, paddingH, paddingW)
      // Be wary of the transpose; we're storing row-major, BLAS is column-major
      // a : im2col(input_image)
      // b : weights
      // m : rows of a == rows of c
      // n : cols of b == cols of c
      // k : cols of a == rows of b
      val a_m = filterSize * filterSize * sample.depth
      val a_n = outputH * outputW
      val b_m = filters
      val b_n = a_m
      val c_m = b_m
      val c_n = a_n
      val m = a_m
      val n = b_n
      val k = a_n
      val y = linalg.gemm("N", "T", m, n, k, field.one, col, weights.toArray, field.one)
      if (bias.nonEmpty) {
        val yb =
          y.grouped(outputH * outputW)
            .toArray
            .zip(bias)
            .flatMap { outcb =>
              val (outputChannel, bias) = outcb
              outputChannel.map(element => field.plus(element, bias))
            }
        sample.update(outputC, outputH, outputW, yb.toVector.map(activate))
      } else {
        sample.update(outputC, outputH, outputW, y.toVector.map(activate))
      }
    }
  }

  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A] = {
    weights
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