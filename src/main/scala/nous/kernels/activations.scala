package nous.kernels

import scala.{specialized => sp}
import scala.reflect.ClassTag

import nous.data._
import spire.algebra._
import spire.math
import spire.math._
import spire.implicits._

object activations {

  def linear[A](x: A) = x

  def tanh[A: Trig](x: A) = math.tanh(x)

  def sigmoid[A: Field](x: A)(implicit t: Trig[A]) =
    1 / (1 + t.exp(implicitly[Field[A]].negate(x)))

  def relu[A: Order](x: A)(implicit f: Field[A]): A = math.max(f.zero, x)

  def softplus[A](x: A)(implicit t: Trig[A]): A = t.log1p(t.exp(x))

}
