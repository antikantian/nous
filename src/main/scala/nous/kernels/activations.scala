package nous.kernels

import nous.linalg.Matrix
import spire.algebra.Trig
import spire.math.Numeric
import spire.implicits._

object activations {

  def relu[A](x: A)(implicit t: Trig[A]): A =
    t.log(1 + t.exp(x))

  def relu[A](mx: Matrix[A]): Matrix[A] =
    mx.map(x => relu(x))

  def softmax[A](input: Matrix[A], output: Matrix[A]): Unit = {

  }

}