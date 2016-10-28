package nous.network
package optimizers

import scala.reflect.ClassTag

import spire.algebra._
import spire.math._
import spire.implicits._

final case class SGD[A: Field: ClassTag](lr: Double = 0.01) extends Optimizer[A] {
  def update(gradient: Array[A], params: Array[A]): Array[A] = {
    params.map(_ * lr) - gradient.map(_ * lr)
  }
}