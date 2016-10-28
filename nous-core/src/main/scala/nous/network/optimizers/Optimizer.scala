package nous.network
package optimizers

trait Optimizer[A] {
  def update(gradient: Array[A], params: Array[A]): Array[A]
}

