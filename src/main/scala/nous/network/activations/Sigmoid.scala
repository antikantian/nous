package nous.network
package activations

import scala.reflect.ClassTag

import fs2._
import nous.network.layers._
import spire.algebra._
import spire.implicits._

final case class Sigmoid[A: Field: Trig: Order: ClassTag](
  implicit m: Module[Vector[A], A]) extends ActivationF[A] {

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x.map(_.map(a => 1 / (1 + Trig[A].exp(Field[A].negate(a)))))
  }

  def backward(grad: Vector[A], : Vector[A]): Vector[A] = {
    // df = f(x) * (1 - f(x))
    // o : Output, so o == f(x)
    // looking for delta, so, calculate (y - t) * f'(x)
    val ov = o.toVector.map(a => a * (Field[A].one - a))
    assert(ov.length == oErr.length, "Output vector doesn't match length of error vector")


    /**
    o.zipWith(oErr) { (sample, err) =>
      sample.map(a => err * a * (Field[A].one - a))
    }
     */


  }

}