package nous.data

import spire.algebra._

class Gradient[A](val k: Int, val m: Int, val n: Int, elems: Vector[A]) {
  val data = elems
  val length = k * m * n
}