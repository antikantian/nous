package nous.kernels

import nous.kernels.convolution._
import org.scalameter._
import org.scalatest.FunSuite
import spire.implicits._
import spire.math.Rational

class ConvolutionKernelTest extends FunSuite {

  test("convolution 1d with rational") {
    val time = measure {
      println("----------")
      println("Convolution 1D with rational")
      val input = Vector[Rational](10, 50, 60, 10, 20, 40, 30)
      val kernel = Vector[Rational](Rational(1, 3), Rational(1, 3), Rational(1, 3))
      val expected = Vector[Rational](20, 40, 40, 30, 20, 30, Rational(2333, 100))

      val calculated = conv1(input, kernel)
      println(s"Input: [${input.mkString(", ")}]")
      println(s"Kernel: [${kernel.mkString(", ")}]")
      println(s"Expected: [${expected.mkString(", ")}]")
      println(s"Calculated: [${calculated.mkString(", ")}]")
      //assert(calculated == expected)
    }
    println(s"Test completed in: $time\n----------\n")
  }

  test("convolution 1d with float") {
    val time = measure {
      println("----------")
      println("Convolution 1D with float")
      val input = Vector[Float](10, 50, 60, 10, 20, 40, 30)
      val kernel = Vector[Float](0.3f, 0.3f, 0.3f)
      val expected = Vector[Float](20f, 40f, 40f, 30f, 20f, 30f, 23.333f)
      val calculated = conv1(input, kernel)
      println(s"Input: [${input.mkString(", ")}]")
      println(s"Kernel: [${kernel.mkString(", ")}]")
      println(s"Expected: [${expected.mkString(", ")}]")
      println(s"Calculated: [${calculated.mkString(", ")}]")
      //assert(calculated == expected)
    }
    println(s"Test completed in: $time\n----------\n")
  }

  test("convolution 1d with double") {
    val time = measure {
      println("----------")
      println("Convolution 1D with double")
      val input = Vector[Double](10, 50, 60, 10, 20, 40, 30)
      val kernel = Vector[Double](0.3d, 0.3d, 0.3d)
      val expected = Vector[Double](20d, 40d, 40d, 30, 20d, 30d, 23.333d)
      val calculated = conv1(input, kernel)
      println(s"Input: [${input.mkString(", ")}]")
      println(s"Kernel: [${kernel.mkString(", ")}]")
      println(s"Expected: [${expected.mkString(", ")}]")
      println(s"Calculated: [${calculated.mkString(", ")}]")
      //assert(calculated == expected)
    }
    println(s"Test completed in: $time\n----------\n")
  }

  test("convolution 1d with int") {
    val time = measure {
      println("----------")
      println("Convolution 1D with int")
      val input = Vector[Int](10, 50, 60, 10, 20, 40, 30)
      val kernel = Vector[Int](3, 1, 2)
      val expected = Vector[Int](20, 40, 40, 30, 20, 30, 23)
      val calculated = conv1(input, kernel)
      println(s"Input: [${input.mkString(", ")}]")
      println(s"Kernel: [${kernel.mkString(", ")}]")
      println(s"Expected: [${expected.mkString(", ")}]")
      println(s"Calculated: [${calculated.mkString(", ")}]")
      //assert(calculated == expected)
    }
    println(s"Test completed in: $time\n----------\n")
  }

  test("naive convolution 2d with int") {
    val time = measure {
      println("----------")
      println("Convolution 2D with int")
      val input = Vector[Int](
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

      val kernel = Vector[Int](
        0,-1, 0,
        0, 0,-1,
        0,-1,-1,

        -1, 1,-1,
        0,-1, 1,
        1, 0, 1,

        1, 1, 0,
        -1,-1, 0,
        1, 1, 1)

      val stride = 2
      val padding = 1
      val calculated = naiveConv2(input, kernel, padding, stride).toVector
      println(s"Input: [${input.mkString(", ")}]")
      println(s"Kernel: [${kernel.mkString(", ")}]")
      //println(s"Expected: [${expected.mkString(", ")}]")
      println(s"Calculated: [${calculated.mkString(", ")}]")
      //assert(calculated == expected)
    }
    println(s"Test completed in: $time\n----------\n")
  }

  test("im2col with int") {
    val time = measure {
      println("----------")
      println("im2col with int")
      val input = Vector[Int](
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

      val channels = 3
      val height = 5
      val width = 5
      val kernel = 3
      val stride = 2
      val pad = 1
      val calculated = im2col(input, channels, height, width, kernel, stride, pad)
      println(s"Input: [${input.mkString(", ")}]")
      //println(s"Expected: [${expected.mkString(", ")}]")
      println(s"Calculated: [${calculated.mkString(", ")}]")
      //assert(calculated == expected)

      val maxCores = Runtime.getRuntime.availableProcessors
      println(s"Available cores: $maxCores")
      (1 to maxCores) foreach { core =>
        val ctime = measure {
          im2col(input, channels, height, width, kernel, stride, pad, Some(core))
        }
        println(s"with $core core(s): $ctime")
      }
    }
    println(s"Test completed in: $time\n----------\n")
  }

  lazy val testInput2D =
    Array[Float](
      1, 1, 1, 1, 1,
      1, 2, 2, 2, 1,
      1, 0, 0, 0, 1,
      1, 3, 3, 3, 1,
      1, 1, 1, 1, 1,

      2, 2, 2, 2, 2,
      2, 1, 1, 1, 2,
      2, 0, 0, 0, 2,
      2, 4, 4, 4, 2,
      2, 2, 2, 2, 2,

      3, 3, 3, 3, 3,
      3, 2, 2, 2, 3,
      3, 0, 0, 0, 3,
      3, 5, 5, 5, 3,
      3, 3, 3, 3, 3)

  lazy val testKernel2D =
    Array[Float](
      -1, 0, 1,
      -1, 1, 0,
      -1, 0, 1,

      1, -1, 0,
      0, -1, 1,
      1, -1, 0,

      0, 0, -1,
      1, 1, -1,
      0, 0, -1)

  lazy val testBias =
    Array[Float](1, 0, 1)

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
}