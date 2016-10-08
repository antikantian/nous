package nous.util

import fs2._
import spire.algebra._
import spire.math._
import spire.random._
import spire.implicits._

object Initializer {

  def uniform[A: Field: Uniform](lower: A, upper: A): Stream[Task, A] = {
    val distribution = Uniform[A](lower, upper)
    Stream.repeatEval(Task.delay(distribution(Generator.rng)))
  }

  /**
   * Y. LeCun, L. Bottou, G. Orr, and K. Mueller.  Efficient BackProp.  In G. Orr and K. Mueller,
   * Neural Networks: tricks of the trade, pages 9-48.  Springer, 1998.
   */
  def lecun[A: NRoot: Uniform](channels: Int, rows: Int, cols: Int)(implicit f: Field[A]): Stream[Task, A] = {
    val scale = ((3 / channels * rows * cols) * f.one).sqrt
    uniform(f.negate(scale), scale)
  }

  /**
   * X. Glorot and Y. Bengio.  Understanding the difficulty of training deep feedforward neural networks.
   * In AISTATS, 2010.
   */
  def glorotUniform[A: NRoot: Uniform](
      samples   : Int,
      channels  : Int,
      rows      : Int,
      cols      : Int)(
      implicit
      f: Field[A]) = {
    val scale = ((2 / channels * rows * cols + samples * rows * cols) * f.one).sqrt
    uniform(f.negate(scale), scale)
  }

}