package nous.kernels

import scala.{ Numeric => Num }

import spire.algebra._
import spire.math._
import spire.implicits._

object loss {

  def mse[A: Field: Num](y: Vector[A], target: Vector[A]): A =
    (target - y).map(_ ** 2).sum / y.length

}