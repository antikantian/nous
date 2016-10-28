package nous.data.tensor

import scala.reflect.ClassTag

final class Sample[A: ClassTag](
    k:                       Int,
    r:                       Int,
    c:                       Int,
    private[nous] val data:  Array[Array[Array[A]]],
    private[nous] val trans: Boolean
) {

  private[nous] def dim2: Array[A] = dim1(0)
  private[nous] def dim1: Array[Array[A]] = data(0)
  private[nous] def dim0: Array[Array[Array[A]]] = data

  val depth = k
  val rows = r
  val cols = c
  val isTransposed = trans
  val strideK = r * c
  val strideR = c

  /**
  def reshape(k: Int, r: Int, c: Int): Sample[A] = {
    val elems =
      Array
        .concat(dim0.foldLeft(new Array[A](this.k * this.r * this.c))((acc, arr) => acc ++ arr.flatten))
        .grouped(r * c).toArray
        .grouped(c).toArray
    new Sample(k, r, c, elems, trans)
  }
   */

  def update(data: Array[Array[Array[A]]]): Sample[A] =
    new Sample(k, r, c, data, trans)

  def update(data: Array[Array[Array[A]]], trans: Boolean): Sample[A] =
    new Sample(k, r, c, data, trans)

  def update(k: Int, r: Int, c: Int, data: Array[Array[Array[A]]], trans: Boolean): Sample[A] =
    new Sample(k, r, c, data, trans)

}