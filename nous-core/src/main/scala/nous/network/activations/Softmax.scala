package nous.network
package activations

import scala.reflect.ClassTag

import nous.data._
import nous.kernels.activations.softmax
import spire.algebra._
import spire.implicits._

final case class Softmax[A: Field: Trig: Order: ClassTag]() extends ActivationF[A] {

  def forward(x: Sample[A, A]): Sample[A, A] = {
    x.update(softmax(x.data))
  }

  def backward(y: NetworkOutput[A], gradient: Array[A]): Array[A] = {
    //val gx = xa.zip(gradient).map(aa => aa._1 * aa._2)
    gradient
  }


}