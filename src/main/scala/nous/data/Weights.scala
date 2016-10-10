package nous.data

import scala.reflect.ClassTag

class Weights[A: ClassTag](val n: Int, val k: Int, r: Int, c: Int, wn: Vector[A]) { self =>
  val data = wn
  val length = data.length

  def map[B: ClassTag](f: A => B): Weights[B] = {
    val wb = data map f
    new Weights(n, k, r, c, wb)
  }

  def update(w: Vector[A]): Weights[A] =
    new Weights(n, k, r, c, w)

  def toArray: Array[A] = data.toArray

  def isEmpty: Boolean = data.isEmpty

  def nonEmpty: Boolean = !isEmpty

}

object Weights {
  def apply[A: ClassTag](n: Int, k: Int, r: Int, c: Int, wn: Vector[A]): Weights[A] = new Weights(n, k, r, c, wn)

  def empty[A: ClassTag]: Weights[A] = new Weights(0, 0, 0, 0, Vector.empty[A])
}