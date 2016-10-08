package nous.kernels

import fs2._
import nous.kernels.convolution._
import org.scalameter._
import org.scalatest.FunSuite
import spire.implicits._
import spire.math.{ConvertableTo, Rational, Numeric}
import spire.random._

class ConvolutionKernelTest extends FunSuite {

  import ConvolutionKernelTest._

  test("convolution 1d with float") {
    val time = measure {
      val input = Vector[Float](10, 50, 60, 10, 20, 40, 30)
      val kernel = Vector[Float](0.3f, 0.3f, 0.3f)
      conv1(input, kernel)
    }
    println(s"conv1 with float took: $time")
  }

  test("convolution 1d with double") {
    val time = measure {
      val input = Vector[Double](10, 50, 60, 10, 20, 40, 30)
      val kernel = Vector[Double](0.3d, 0.3d, 0.3d)
      conv1(input, kernel)
    }
    println(s"conv1 with double took: $time")
  }

  test("im2col with float") {
    val channels = 3
    val height = 5
    val width = 5
    val kernel = 3
    val stride = 2
    val pad = 1
    val input5x5x3float = makeRandom[Float](5, 5, 3)
    val input28x28x32float = makeRandom[Float](28, 28, 32)

    val time1 = measure {
      im2col(input5x5x3float, channels, height, width, kernel, stride, stride, pad, pad)
    }
    println(s"im2colImpl2 5x5x3 with float: $time1")

    val time2 = measure {
      im2col(input28x28x32float, 32, 28, 28, 3, 1, 1, 0, 0)
    }
    println(s"im2colImpl2 28x28x32 with float: $time2")
  }
}

object ConvolutionKernelTest {
  val intInput5x5x1 =
    Vector[Int](
      0, 1, 1, 1, 0,
      1, 0, 1, 1, 1,
      1, 2, 0, 0, 2,
      1, 2, 1, 1, 1,
      0, 0, 2, 0, 0)

  val intInput5x5x2 =
    Vector[Int](
      0, 1, 1, 1, 0,
      1, 0, 1, 1, 1,
      1, 2, 0, 0, 2,
      1, 2, 1, 1, 1,
      0, 0, 2, 0, 0,

      2, 0, 0, 2, 2,
      0, 0, 2, 0, 2,
      2, 2, 1, 0, 1,
      2, 1, 2, 2, 2,
      2, 0, 0, 2, 1)

  val intInput5x5x3 =
    Vector[Int](
      0, 1, 1, 1, 0,
      1, 0, 1, 1, 1,
      1, 2, 0, 0, 2,
      1, 2, 1, 1, 1,
      0, 0, 2, 0, 0,

      2, 0, 0, 2, 2,
      0, 0, 2, 0, 2,
      2, 2, 1, 0, 1,
      2, 1, 2, 2, 2,
      2, 0, 0, 2, 1,

      0, 2, 0, 0, 0,
      0, 2, 1, 1, 1,
      1, 2, 1, 0, 2,
      1, 1, 2, 2, 1,
      2, 1, 1, 0, 0)

  val intKernel3x3x1 =
    Vector[Int](
      0,-1, 0,
      0, 0,-1,
      0,-1,-1)

  val intKernel3x3x2 =
    Vector[Int](
      0,-1, 0,
      0, 0,-1,
      0,-1,-1,

     -1, 1,-1,
      0,-1, 1,
      1, 0, 1)

  val intKernel3x3x3 =
    Vector[Int](
      0,-1, 0,
      0, 0,-1,
      0,-1,-1,

     -1, 1,-1,
      0,-1, 1,
      1, 0, 1,

      1, 1, 0,
     -1,-1, 0,
      1, 1, 1)

  def makeRandom[A](rows: Int, cols: Int, channels: Int)(implicit ev: Numeric[A], ev1: ConvertableTo[A]) = {
    val lower = -5.0
    val upper = 5.0
    val distribution = Uniform(lower, upper)
    Stream
      .repeatEval(Task.delay(ev.fromDouble(distribution.apply(Generator.rng))))
      .take(rows * cols * channels)
      .runLog
      .unsafeRun
  }
}