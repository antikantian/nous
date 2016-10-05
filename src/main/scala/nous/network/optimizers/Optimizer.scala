package nous.network.optimizers

import spire.algebra._
import spire.math._
import spire.implicits._

sealed trait Optimizer

case class SGD(lr: Double = 0.01, momentum: Double = 0.9, decay: Double = 0, nesterov: Boolean = false) extends Optimizer {
  def compute[A](dx: A, x: A, w: A)(ev: Numeric[A]) =
    ev.times(ev.minus(w, ev.fromDouble(lr)), ev.times(ev.plus(dx, ev.fromDouble(decay)), w))
}