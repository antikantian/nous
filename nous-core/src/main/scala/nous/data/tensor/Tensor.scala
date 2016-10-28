package nous.data.tensor

import scala.{specialized => sp}

final class Tensor[@sp(Double, Float) A](
    s:                       Int,
    k:                       Int,
    r:                       Int,
    c:                       Int,
    private[nous] val data:  Array[Array[Array[Array[A]]]]
) {

  type Row = Array[A]
  type Column = Array[A]
  type K = Array[Array[A]]
  type Sample = Array[Array[Array[A]]]

  private[nous] def dim3: Array[A] = dim2(0)
  private[nous] def dim2: Array[Array[A]] = dim1(0)
  private[nous] def dim1: Array[Array[Array[A]]] = data(0)
  private[nous] def dim0: Array[Array[Array[Array[A]]]] = data

  val samples = s
  val depth = k
  val rows = r
  val cols = c
  val size = s * k * r * c

  def isEmpty: Boolean = data.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def fail(m: String) = throw new NoSuchElementException(m)

}

object Tensor {

}