package nous.util

import fs2._
import spire.algebra._
import spire.math._
import spire.random._
import spire.implicits._

object Initializer {

  def uniform[A: Numeric: Uniform](lower: A, upper: A): Stream[Task, A] = {
    val distribution = Uniform[A](lower, upper)
    Stream.repeatEval(Task.delay(distribution(Generator.rng)))
  }

  /**
   * Y. LeCun, L. Bottou, G. Orr, and K. Mueller.  Efficient BackProp.  In G. Orr and K. Mueller,
   * Neural Networks: tricks of the trade, pages 9-48.  Springer, 1998.
   */
  def lecun[A: ConvertableTo: Numeric: Uniform](channels: Int, rows: Int, cols: Int): Stream[Task, A] = {
    val scale = implicitly[ConvertableTo[A]].fromDouble(sqrt(3 / channels * rows * cols))
    uniform(implicitly[Numeric[A]].negate(scale), scale)
  }

  /**
   * X. Glorot and Y. Bengio.  Understanding the difficulty of training deep feedforward neural networks.
   * In AISTATS, 2010.
   */
  def glorotUniform[A](
      samples   : Int,
      channels  : Int,
      rows      : Int,
      cols      : Int)(
      implicit
      ev  : ConvertableTo[A] with Numeric[A] with Uniform[A]) = {
    val scale = ev.fromDouble(sqrt(2 / channels * rows * cols + samples * rows * cols))
    uniform(ev.negate(scale), scale)
  }

}