package nous.network
package activations

import scala.reflect.ClassTag

import nous.network.layers._
import spire.algebra._

final case class Softplus[A: Field: Trig: ClassTag]() extends ActivationF[A] {
  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x.map(_.map(a => Trig[A].log1p(Trig[A].exp(a))))
  }

  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A] = {
    Vector.empty[A]
  }

}