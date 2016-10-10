package nous.kernels

import scala.{specialized => sp}
import scala.reflect.ClassTag

import nous.data._
import spire.algebra._
import spire.math._
import spire.implicits._

object activations {

  def linear[A](x: A) = x

  def relu[A: Order](x: A)(implicit f: Field[A]): A = spire.math.max(f.zero, x)

  def sigmoid[A: Field](x: A)(implicit t: Trig[A]) =
    1 / (1 + t.exp(implicitly[Field[A]].negate(x)))

  def softmax[A: Field : NumberTag : Order : Trig](x: Vector[A])(
      implicit vs: NormedVectorSpace[Vector[A], A]): Vector[A] = {

    NumberTag[A].hasNegativeInfinity map { inf =>
      x map { a =>
        val ma = max(inf, a)
        exp(Field[A].minus(a, ma))
      }
    } match {
      case Some(vector) => vector.normalize
      case _ => x.normalize
    }
  }

  def softplus[A](x: A)(implicit t: Trig[A]): A = t.log1p(t.exp(x))

  def tanh[A: Trig](x: A) = spire.math.tanh(x)

}
