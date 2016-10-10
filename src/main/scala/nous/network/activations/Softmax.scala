package nous.network
package activations

import scala.reflect.ClassTag

import nous.network.layers._
import spire.algebra._
import spire.math._
import spire.implicits._

final class Softmax[A: Field: Trig: Order: NumberTag: ClassTag](
  implicit vs: NormedVectorSpace[Vector[A], A]) extends ActivationF[A] {

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x map { sample =>
      val snorm = sample mapChannels { channel =>
        val cnorm = NumberTag[A].hasNegativeInfinity map { inf =>
          channel.data map { a =>
            val ma = max(inf, a)
            exp(Field[A].minus(a, ma))
          }
        } match {
          case Some(vector) => vector.normalize
          case _ => channel.data.normalize
        }
        channel.update(cnorm)
      }
      snorm
    }
  }

  def backward(x: LayerInput[A], gradient: Vector[A]): Vector[A] = {
    Vector.empty[A]
  }


}