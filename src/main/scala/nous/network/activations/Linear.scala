package nous.network
package activations

import scala.reflect.ClassTag

import nous.data._

final case class Linear[A: ClassTag]() extends ActivationF[A] {
  def forward(x: Sample[A, A]): Sample[A, A] = {
    x
  }

  def backward(y: NetworkOutput[A], gradient: Vector[A]): Vector[A] = {
    Vector.empty[A]
  }
}