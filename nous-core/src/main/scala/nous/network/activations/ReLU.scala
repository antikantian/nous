package nous.network
package activations

import scala.reflect.ClassTag

import nous.data._
import spire.algebra._
import spire.math._

final case class ReLU[A: Field: Order: ClassTag]() extends ActivationF[A] {
  def forward(x: Sample[A, A]): Sample[A, A] = {
    x.map(a => max(Field[A].zero, a))
  }

  def backward(y: NetworkOutput[A], gradient: Array[A]): Array[A] = {
    Array.empty[A]
  }

}