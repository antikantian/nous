package nous.network
package optimizers

trait Optimizer[A] {
  def update(lr: Double, W: Vector[A], dW: Vector[A]): Vector[A]
}

