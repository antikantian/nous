package nous.network
package activations

import scala.reflect.ClassTag

import nous.data._
import nous.kernels.activations.softmax
import spire.algebra._

final case class Softmax[A: Field: Trig: Order: ClassTag]() extends ActivationF[A] {

  def forward(x: Sample[A, A]): Sample[A, A] = {
    x.update(softmax(x.data))
  }

  def backward(y: NetworkOutput[A], deltas: Vector[A]): Vector[A] = {
    deltas
  }


}