package nous.network
package activations

import scala.reflect.ClassTag

import nous.network.layers._
import spire.algebra._
import spire.math._
import spire.implicits._

final case class ReLU[A: Field: Order: ClassTag]() extends ActivationF[A] {
  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x.map(_.map(a => max(Field[A].zero, a)))
  }

  def backward(x: LayerInput[A], gradient: Vector[A]): Vector[A] = {
    Vector.empty[A]
  }

}