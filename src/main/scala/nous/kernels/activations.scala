package nous.kernels

import scala.{specialized => sp}

import spire.algebra._
import spire.math
import spire.math.{ConvertableFrom, Numeric}
import spire.implicits._

object activations {

  def linear[A](x: A) = x

  def logistic[A: Field: Trig](x: A) = 1 / (1 + math.exp(-x))

  def tanh[A: Field: Trig](x: A) = math.tanh(x)

  def sigmoid[A: Field: Trig](x: A)(implicit ev: ConvertableFrom[A]) =
    1 / (1 + math.pow(math.e, -1 * ev.toDouble(x)))

}