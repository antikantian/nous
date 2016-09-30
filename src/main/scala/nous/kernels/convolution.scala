package nous.kernels

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.{Vector => SVector}
import scala.{ Numeric => Num }

import cats.data._
import fs2._
import nous.data._
import nous.data.Coord._
import spire.algebra.Field
import spire.math._
import spire.implicits._

object convolution {

  /**
   * Convolution in 1D: y[i] = Σh[j] ⋅ x[i - j], from j = 0 to M - 1
   */
  def conv1[A: ClassTag: Num](input: Vector[A], kernel: Vector[A])(implicit ev: Numeric[A]) = {
    val hs = kernel
    val xs = input
    val pad = Vector.fill(hs.length - 1)(ev.zero)
    val ts = pad ++ xs ++ pad
    Stream
      .emits(ts)
      .sliding(hs.length)
      .map(vec => vec.zip(hs.reverse).map(a => a._1 * a._2).sum)
      .toVector
  }

  def conv2[A: ClassTag](input: Vector[A], kernel: Vector[A], padding: Int, stride: Int)(implicit ev: Numeric[A]): Vector[A] = {
    // Static sizes
    val inputHW = sqrt(input.length).toInt
    val kernelHW = sqrt(kernel.length).toInt
    val rowSizeAfterPad = inputHW + kernelHW - 1
    val rowSizeStrided = rowSizeAfterPad + stride
    val passSize = kernelHW * rowSizeStrided
    val inputSizePaddedStrided = (rowSizeStrided * inputHW) + (rowSizeStrided * 2)

    @tailrec
    def buildKernelStream(
        zip: Int, totalZips: Int,
        chunkedKernel: Stream[Pure, Vector[A]],
        acc: Stream[Pure, Vector[A]]): Stream[Pure, A] = {
      if (zip < totalZips) {
        buildKernelStream(zip + 1, totalZips, chunkedKernel, acc.zipWith(chunkedKernel)(_ ++ _))
      } else acc.flatMap(k => Stream.emits(k)).pure
    }

    @tailrec
    def buildStream(pass: Int, totalPasses: Int, rows: Stream[Pure, A], acc: Stream[Pure, A]): Stream[Pure, A] = {
      if (pass < totalPasses) {
        val newAcc =
          acc ++
            rows
              .drop(pass * rowSizeStrided * stride)
              .dropRight(inputSizePaddedStrided - (pass * stride * rowSizeStrided) - passSize)
        //print(s"Dropped left: ${pass * rowSizeStrided * stride}, dropped right: ${inputSizePaddedStrided - (pass * stride * rowSizeStrided) - passSize}, row size: $rowSizeStrided\n")
        buildStream(pass + 1, totalPasses, rows, newAcc)
      } else acc
    }

    // Build helpers
    val padHelper = Stream.constant(ev.zero)
    val paddedRow = padHelper.vectorChunkN(rowSizeAfterPad).take(1)
    val rowInterleavePadding = Stream.emits(input).vectorChunkN(inputHW).map(vec => ev.zero +: vec :+ ev.zero)
    val rowsAfterPadding = paddedRow ++ rowInterleavePadding ++ paddedRow
    val rowStream = rowsAfterPadding.map(_.sliding(kernelHW, stride).toVector.flatten).flatMap(row => Stream.emits(row)).pure
    val kernelChunked = Stream.emits(kernel).vectorChunkN(kernelHW).pure

    // Prepared input (finite) and kernel (infinite) streams
    val kernelStream = buildKernelStream(0, (rowSizeStrided / kernelHW) - 1, kernelChunked, kernelChunked)
    val dataStream = buildStream(0, rowSizeStrided / kernelHW, rowStream, Stream.empty[Pure, A])

    // Zip together, multiply elements, add elements in chunks equal to kernel width, do some maps, zips, et viola...
    dataStream
      .zipWith(kernelStream.repeat)((in, kn) => in * kn)
      .vectorChunkN(kernelHW)
      .map(ev.sum(_))
      .vectorChunkN(kernel.length)
      .map(kernelLengthChunk =>
        kernelLengthChunk
          .grouped(kernelHW)
          .toVector
          .flatMap(_.zipWithIndex)
          .sortWith(_._2 <= _._2)
          .unzip._1
          .grouped(kernelHW)
          .map(ev.sum(_))
          .toVector)
      .flatMap(Stream.emits)
      .pure
      .toVector
  }

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