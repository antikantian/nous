package nous.network
package activations

import scala.reflect.ClassTag

import nous.data._
import spire.algebra._

final case class Softplus[A: Field: Trig: ClassTag]() extends ActivationF[A] {
  def forward(x: Sample[A, A]): Sample[A, A] = {
    x.map(a => Trig[A].log1p(Trig[A].exp(a)))
  }

  def backward(y: NetworkOutput[A], gradient: Vector[A]): Vector[A] = {
    Vector.empty[A]
  }

}