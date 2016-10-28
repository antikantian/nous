package nous.network
package loss

trait LossF[A] {
  def forward(y: NetworkOutput[A]): Array[A]
  def backward(predicted: Array[A], target: Array[A]): Array[A]
}