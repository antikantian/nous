package nous.kernels

import scala.{Vector => SVector}

import fs2._
import nous.data._
import nous.data.Coord._
import spire.math.Numeric

object convolution {

  /**

  private[nous] def im2col[A](
      input       : Sample[A],
      filterRows  : Int,
      filterCols  : Int,
      strideH     : Int,
      strideW     : Int,
      paddingH    : Int,
      paddingW    : Int)(
      implicit ev : Numeric[A]): Matrix[A] = {

    val outputRows = 1 + (input.rows + 2 * paddingH - filterRows) / strideH
    val outputCols = 1 + (input.rows + 2 * paddingW - filterCols) / strideW
    val maxRows = input.rows + paddingH - (filterRows - 1)
    val maxCols = input.cols + paddingW - (filterCols - 1)
    val boundary = input.boundary

    val rowStream = Stream.range(-paddingH, maxRows, -paddingH + strideH)
    val colStream = Stream.range(-paddingW, maxCols, -paddingW + strideW)
    val kStream = Stream.range(0, input.depth)
    val yStream = Stream.range(0, filterRows)
    val xStream = Stream.range(0, filterCols)

    val coordStream =
      for {
        r <- rowStream
        c <- colStream
        k <- kStream
        y <- yStream
        x <- xStream
      } yield Im2Col(r, c, k, y, x)

    val mapped = coordStream map { coord =>
      if (boundary.contains(coord.c + coord.x, coord.r + coord.y))
        input.values((coord.k * input.rows + coord.y) * input.cols + coord.x)
      else
         ev.zero
    }
    new Matrix(outputRows * outputCols, input.depth * filterRows * filterCols)(mapped.toVector.toArray)
  }

  private[nous] def col2im[A: Numeric](
      input       : Matrix[A],
      channels    : Int,
      rows        : Int,
      cols        : Int,
      filterRows  : Int,
      filterCols  : Int,
      strideH     : Int,
      strideW     : Int,
      paddingH    : Int,
      paddingW    : Int): Sample[A] = {

    val maxRows = rows + paddingH - (filterRows - 1)
    val maxCols = cols + paddingW - (filterCols - 1)
    val boundary = Rectangle(0, 0, cols - 1, rows - 1)

    val rowStream = Stream.range(-paddingH, maxRows, -paddingH + strideH)
    val colStream = Stream.range(-paddingW, maxCols, -paddingW + strideW)
    val kStream = Stream.range(0, channels)
    val yStream = Stream.range(0, filterRows)
    val xStream = Stream.range(0, filterCols)

    val coordStream =
      for {
        r <- rowStream
        c <- colStream
        k <- kStream
        y <- yStream
        x <- xStream
      } yield Im2Col(r, c, k, y, x)

    val zipmapped = coordStream
      .zip(Stream.emits(input.data))
      .filter(ca => boundary.contains(ca._1.c + ca._1.x, ca._1.r + ca._1.y))
      .map(_._2)

    new Sample(channels, rows, cols)(zipmapped.toVector.toArray)
  }

  private[nous] def conv2d[A](
      input     : Tensor[A],
      filters   : Tensor[A],
      strideH   : Int,
      strideW   : Int,
      paddingH  : Int,
      paddingW  : Int): Tensor[A] = {

    // input.samples == filters.samples

    val outputDepth = filters.samples.value
    val outputRows = 1 + (input.rows + 2 * paddingH - filters.rows) / strideH
    val outputCols = 1 + (input.cols + 2 * paddingW - filters.cols) / strideW

    val kernelMatrix = filters.asMatrix
    val outSamples = input mapSamples { sample =>
      val mat = im2col(sample, filters.rows, filters.cols, strideH, strideW, paddingH, paddingW)
      val out = kernelMatrix * mat.transpose
      new Sample(outputDepth, outputRows, outputCols)(out.data)
    }
    outSamples
  }

  private[nous] def dataGradient[A](
      dataGrad  : Tensor[A],
      filters   : Tensor[A],
      channels  : Int,
      rows      : Int,
      cols      : Int,
      strideH   : Int,
      strideW   : Int,
      paddingH  : Int,
      paddingW  : Int): Tensor[A] = {

    val kernelMatrix = filters.asMatrix

    val outGradient = dataGrad mapSamples { sample =>
      val mat = sample.asMatrix.transpose * kernelMatrix
      col2im(mat, channels, rows, cols, filters.rows, filters.cols, strideH, strideW, paddingH, paddingW)
    }
    outGradient
  }

  private[nous] def filterGradient[A](
      dataGrad: Tensor[A],
      data: Tensor[A],
      filterRows: Int,
      filterCols: Int,
      strideH: Int,
      strideW: Int,
      paddingH: Int,
      paddingW: Int): Tensor[A] = {

    dataGrad mapSamples { sample =>
      im2col(sample, filterRows, filterCols, strideH, strideW, paddingH, paddingW)

    }

  }

   */

}