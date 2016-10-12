package nous.network
package activations

import nous.data._
import nous.network.layers._

trait ActivationF[A] {
  def forward(x: Sample[A, A]): Sample[A, A]
  def backward(y: NetworkOutput[A], gradient: Vector[A]): Vector[A]
}
