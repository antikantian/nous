package nous.network
package activations

import scala.reflect.ClassTag

import nous.data._
import spire.algebra._
import spire.implicits._

final case class Sigmoid[A: Field: Trig: Order: ClassTag](
  implicit m: Module[Vector[A], A]) extends ActivationF[A] {

  def forward(x: Sample[A, A]): Sample[A, A] = {
    x.map(a => 1 / (1 + Trig[A].exp(Field[A].negate(a))))
  }

  def backward(y: NetworkOutput[A], gradient: Vector[A]): Vector[A] = {
    gradient
  }

}