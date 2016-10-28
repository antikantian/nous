package nous.network
package activations

import scala.reflect.ClassTag

import nous.data.Sample
import spire.algebra._
import spire.math._

final case class Tanh[A: Field: Trig: ClassTag]() extends ActivationF[A] {
  def forward(x: Sample[A, A]): Sample[A, A] = {
    x.map(a => tanh(a))
  }

  def backward(y: NetworkOutput[A], gradient: Array[A]): Array[A] = {
    Array.empty[A]
  }
}