package nous.kernels

import scala.reflect.ClassTag

import cats.Eval
import spire.algebra._
import spire.implicits._

object loss {

  def crossEntropy[A: ClassTag](y: Array[A], p: Array[A])(implicit f: Field[A], t: Trig[A]): Array[A] =
    y.zip(p.map(t.log)).map(a => f.negate(a._1 * a._2)) :/ y.length

  def crossEntropyD[A: Field: ClassTag](p: Array[A], y: Array[A]): Array[A] =
    Eval.now(y.length).map(ylen => (p - y).map(_ * (1.0 / ylen))).value

  def euclidean[A: Field: ClassTag](y: Array[A], p: Array[A]): Array[A] =
    Array(0.5 * (p - y).reduceLeft(_ + _) ** 2)

  def euclideanD[A: Field: ClassTag](prediction: Array[A], target: Array[A]): Array[A] = {
    prediction - target
  }

}