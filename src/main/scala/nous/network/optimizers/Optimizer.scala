package nous.network
package optimizers

trait Optimizer[A] {
  def update(gradient: Vector[A], params: Vector[A]): Vector[A]
}

