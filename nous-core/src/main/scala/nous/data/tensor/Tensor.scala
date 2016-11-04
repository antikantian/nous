package nous.data.tensor

import scala.{specialized => sp}

class Tensor[@sp(Double, Float) A](s: Int, k: Int, m: Int, n: Int) {
  val samples = s
  val depth = k
  val rows = m
  val cols = n
  val size = samples * depth * rows * cols

  def isEmpty: Boolean = data.isEmpty
  def nonEmpty: Boolean = !isEmpty

}

object Tensor {

}