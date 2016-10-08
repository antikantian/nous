package nous.kernels

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.mutable.ParArray
import scala.reflect.ClassTag
import scala.{Vector => SVector}
import scala.{Numeric => Num}

import cats.data._
import fs2._
import nous.data._
import nous.data.Coord._
import spire.algebra._
import spire.math._
import spire.implicits._

object convolution {

  def pool[A](
      input     : Channel[A],
      winH      : Int,
      winW      : Int,
      strideH   : Int,
      strideW   : Int,
      paddingH  : Int,
      paddingW  : Int,
      maxpool   : Boolean)(
      implicit
      ord       : Order[A],
      field     : Field[A],
      ct        : ClassTag[A]): Channel[A] = {

    val inputH = input.height
    val inputW = input.width

    val pooledH = (inputH + 2 * paddingH - winH) / strideH + 1
    val pooledW = (inputW + 2 * paddingW - winW) / strideW + 1

    val outArray = ct.newArray(pooledH * pooledW)

    val w_offset = winW / 2 - paddingW
    val h_offset = winH / 2 - paddingH

    cfor(0)(_ < pooledH, _ + 1) { h =>
      cfor(0)(_ < pooledW, _ + 1) { w =>
        val overlay = Rectangle.centered(w * strideW + w_offset, h * strideH + h_offset, winW, winH)
        val window = input.subchannel(overlay)

        if (maxpool)
          outArray.update(h * pooledW + w, window.tail.foldLeft(window.head)(ord.max))
        else
          outArray.update(h * pooledW + w, window.foldLeft(field.zero)(_ + _) / window.size)
      }
    }
    Channel(pooledH, pooledW, outArray.toVector)
  }

  def im2col[A](
      input     : Vector[A],
      channels  : Int,
      height    : Int,
      width     : Int,
      kernel    : Int,
      strideH   : Int,
      strideW   : Int,
      paddingH  : Int,
      paddingW  : Int)(
      implicit
      field     : Field[A],
      ct        : ClassTag[A]): Array[A] = {

    val height_col = (height + 2 * paddingH - kernel) / strideH + 1
    val width_col = (width + 2 * paddingW - kernel) / strideW + 1
    val channels_col = channels * kernel * kernel

    val outArray = ct.newArray(height_col * width_col * channels_col)

    cfor(0)(_ < channels_col, _ + 1) { c =>
      val w_offset = c % kernel
      val h_offset = (c / kernel) % kernel
      val c_im = c / kernel / kernel
      cfor(0)(_ < height_col, _ + 1) { h =>
        cfor(0)(_ < width_col, _ + 1) { w =>
          val h_pad = h * strideH - paddingH + h_offset
          val w_pad = w * strideH - paddingW + w_offset
          if (h_pad >= 0 && h_pad < height && w_pad >= 0 && w_pad < width)
            outArray.update((c * height_col + h) * width_col + w, input((c_im * height + h_pad) * width + w_pad))
          else
            field.zero
        }
      }
    }
    outArray
  }

  def col2im[A](
      input     : Vector[A],
      channels  : Int,
      height    : Int,
      width     : Int,
      kernel    : Int,
      strideH   : Int,
      strideW   : Int,
      paddingH  : Int,
      paddingW  : Int)(
      implicit
      field     : Field[A],
      ct        : ClassTag[A]): Array[A] = {

    val height_col = (height + 2 * paddingH - kernel) / strideH + 1
    val width_col = (width + 2 * paddingW - kernel) / strideW + 1
    val channels_col = channels * kernel * kernel

    val outArray = ct.newArray(height * width * channels)

    cfor(0)(_ < channels_col, _ + 1) { c =>
      val w_offset = c % kernel
      val h_offset = (c / kernel) % kernel
      val c_im = c / kernel / kernel
      cfor(0)(_ < height_col, _ + 1) { h =>
        cfor(0)(_ < width_col, _ + 1) { w =>
          val h_pad = h * strideH - paddingH + h_offset
          val w_pad = w * strideW - paddingW + w_offset
          if (h_pad >= 0 && h_pad < height && w_pad >= 0 && w_pad < width) {
            val pv = outArray((c_im * height + h_pad) * width + w_pad)
            outArray.update((c_im * height + h_pad) * width + w_pad, pv + input((c * height_col + h) * width_col + w))
          }
        }
      }
    }
    outArray
  }

  /**
   * Convolution in 1D: y[i] = Σh[j] ⋅ x[i - j], from j = 0 to M - 1
   */
  def conv1[A: ClassTag](input: Vector[A], kernel: Vector[A])(implicit ev: Numeric[A]) = {
    val hs = kernel
    val xs = input
    val pad = Vector.fill(hs.length - 1)(ev.zero)
    val ts = pad ++ xs ++ pad
    ts.sliding(hs.length).map(vec => ev.sum(vec.zip(hs.reverse).map(a => a._1 * a._2)))
  }

  def very_naive_conv2[A: ClassTag](input: Vector[A], kernel: Vector[A], padding: Int, stride: Int)(implicit ev: Numeric[A]): Stream[Pure, A] = {
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
  }

}