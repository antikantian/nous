package nous.network
package loss

trait LossF[A] {
  def forward(y: NetworkOutput[A]): Vector[A]
  def backward(y: Vector[A], p: Vector[A]): Vector[A]
}