package nous.network
package activations

import nous.network.layers._

trait ActivationF[A] {
  def forward(x: LayerInput[A]): LayerOutput[A]
  def backward(y: NetworkOutput[A], g: Vector[A]): Vector[A]
}
