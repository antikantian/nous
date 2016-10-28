package nous.network
package activations

import nous.data._

trait ActivationF[A] {
  def forward(x: Sample[A, A]): Sample[A, A]
  def backward(y: NetworkOutput[A], gradient: Array[A]): Array[A]
}
