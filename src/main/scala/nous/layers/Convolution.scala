package nous.layers

import scala.language.higherKinds

import cats._
import cats.data.Kleisli
import fs2._
import nous.data._
import nous.free.tensor._
import nous.util.exception._

/**

private[nous] class Convolution[A](
    _numFilters: Int,
    _kernelRows: Int,
    _kernelCols: Int,
    _strideH: Int,
    _strideW: Int,
    _inputDepth: Int,
    _weights: Tensor[A],
    _bias: Vector[A],
    _compiler: TensorOp[A] ~> Id)
  extends Layer[A] {

  require(_weights.samples.value == _numFilters, "The number of weights in this layer must be equal to the number of filters.")
  require(_bias.length == _numFilters, "The bias vector must be sized according to the number of filters.")

  val filters = _numFilters
  val rows = _kernelRows
  val cols = _kernelCols
  val strideH = _strideH
  val strideW = _strideW
  val paddingH: Int = if (strideH != 1) 0 else rows / 2
  val paddingW: Int = if (strideW != 1) 0 else cols / 2

  val compiler = _compiler
  val weights = _weights
  val bias = _bias

  val numInputs = rows * cols * _inputDepth
  val numOutputs = filters
  val numWeights = numInputs * filters
  val numParams = numWeights + bias.length

  // Size of weights tensor = (filters, inputDepth, rows, cols)

  def forward(input: Tensor[A]): Task[Tensor[A]] = Task.delay {
    val output = convolve((strideW, strideH), (paddingW, paddingH), weights, filters, weights.depth, rows, cols, input)
    output.foldMap(compiler)
  }

  def backward(input: Tensor[A], gradient: Tensor[A]): Task[(Tensor[A], Tensor[A])] = Task.delay {

  }
}

object Convolution {

  def apply[A](
      filters   : Int,
      rows      : Int,
      cols      : Int,
      strideH   : Int,
      strideW   : Int,
      compiler  : TensorOp[A] ~> Id,
      weights   : Option[Tensor[A]],
      context   : LayerContext[A]): Task[Convolution[A]] = Task.delay {

    val inputSize = rows * cols * context.input.depth
    val weight = weights.getOrElse(Tensor.uniform[A](inputSize * filters + filters, 1, 1, 1))

    new Convolution[A](filters, rows, cols, strideH, strideW, weight, compiler)
  }

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

*/