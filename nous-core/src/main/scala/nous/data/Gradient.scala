package nous.data

import scala.reflect.ClassTag

final class Gradient[A: ClassTag](val r: Int, val c: Int, elems: Array[A]) {
  val data = elems

  def update(m: Int, n: Int, g: Array[A]): Gradient[A] = new Gradient(r, c, g)
}