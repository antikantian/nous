package nous.kernels

import scala.{ Numeric => Num }

import cats.Eval
import spire.algebra._
import spire.implicits._

object loss {

  def crossEntropy[A](y: Vector[A], p: Vector[A])(implicit f: Field[A], t: Trig[A]): Vector[A] =
    y.zip(p.map(t.log)).map(a => f.negate(a._1 * a._2)) :/ y.length

  def crossEntropyD[A: Field](y: Vector[A], p: Vector[A]): Vector[A] =
    Eval.now(y.length).map(ylen => (p - y).map(_ * (1.0 / ylen))).value

  def euclidean[A: Field](y: Vector[A], p: Vector[A]): Vector[A] =
    Vector(0.5 * (p - y).reduceLeft(_ + _) ** 2)

  def euclideanD[A: Field](y: Vector[A], p: Vector[A]): Vector[A] = p - y

}