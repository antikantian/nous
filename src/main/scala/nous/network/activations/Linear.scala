package nous.network
package activations

import scala.reflect.ClassTag

import nous.network.layers._

final case class Linear[A: ClassTag]() extends ActivationF[A] {
  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x
  }

  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A] = {
    Vector.empty[A]
  }
}