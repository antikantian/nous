package nous.kernels

import scala.{ Numeric => Num }

import spire.algebra._
import spire.math._
import spire.implicits._

object loss {

  def mse[A](pred: Vector[A], target: Vector[A])(implicit f: Field[A]): A =
    (pred - target).map(a => f.pow(a, 2)).foldLeft(f.zero)(_ + _) / pred.size

  def mae[A](pred: Vector[A], target: Vector[A])(implicit ev: Signed[A], f: Field[A]): A =
    (pred - target).map(ev.abs).foldLeft(f.zero)(_ + _) / pred.size

}