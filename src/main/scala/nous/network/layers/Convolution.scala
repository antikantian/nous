package nous.network.layers

import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

import cats._
import fs2._
import nous.data._
import nous.kernels.convolution._
import nous.kernels.matrix
import nous.network._
import nous.util.Blas
import definitions._
import nous.util.exception._
import shapeless.syntax.std.tuple._
import spire.algebra._
import spire.math._

class Convolution[A: ClassTag: Numeric: TypeTag](cdef: ConvDefinition[A], input: Shape4D, w: Vector[A])(
    implicit blas: Blas[A]) extends HiddenLayer[A] { self =>

  val definition = cdef
  val weights = w
  val bias =
    if (cdef.bias)
      Vector.fill(definition.filters)(zero)
    else
      Vector.empty[A]

  val filters = definition.filters
  val filterSize = definition.height
  val height = definition.height
  val width = definition.width
  val stride = definition.stride
  val padding = definition.padding

  val numBiases = bias.length
  val numWeights = weights.length
  val numParameters = numWeights + numBiases

  val inputN = input.samples
  val inputC = input.channels
  val inputH = input.height
  val inputW = input.width

  val outputN = input.samples
  val outputC = filters
  val outputH = Convolution.getOutputSize(inputH, filterSize, stride, padding)
  val outputW = outputH
  val outputShape = Shape4D(outputN, outputC, outputH, outputW)

  private[network] def zero(implicit ev: Numeric[A]): A = ev.zero
  private[network] def one(implicit ev: Numeric[A]): A = ev.one

  def activation(fx: Vector[A]): Vector[A] =
    fx map cdef.activation

  def forward(x: LayerInput[A])(implicit ev: Field[A]): LayerOutput[A] = {
    x map { sample =>
      val col = im2col(sample.data, sample.channels, sample.height, sample.width, filterSize, stride, padding)
      // Be wary of the transpose; we're storing row-major, BLAS is column-major
      // m : rows of a == rows of c
      // n : cols of b == cols of c
      // k : cols of a == rows of b
      //val m = outputH * outputW
      //val n = filters
      //val k = filterSize * filterSize * sample.depth
      //val y = blas.gemm("N", "T", m, n, k, one, col, weights.toArray, one)
      val m = filters
      val n = outputH * outputW
      val k = filterSize * filterSize * sample.channels
      val y = blas.gemm("N", "T", m, n, k, one, col, weights.toArray, one)
      if (bias.nonEmpty) {
        val yb =
          y.grouped(outputH * outputW)
            .toArray
            .zip(bias)
            .flatMap { outcb =>
              val (outputChannel, bias) = outcb
              outputChannel.map(element => ev.plus(element, bias))
            }
        sample.update(outputC, outputH, outputW, activation(yb.toVector))
      } else {
        sample.update(outputC, outputH, outputW, activation(y.toVector))
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