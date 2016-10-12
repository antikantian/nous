package nous.network
package optimizers

import spire.algebra._
import spire.math._
import spire.implicits._

final case class SGD[A: Field](lr: Double = 0.01) extends Optimizer[A] {
  def update(gradient: Vector[A], params: Vector[A]): Vector[A] = {
    params.map(_ * lr) - gradient.map(_ * lr)
  }
}