package nous.network
package loss

import scala.reflect.ClassTag

import nous.kernels.loss._
import spire.algebra._

final case class MeanSquaredError[A: Field: ClassTag](implicit m: Module[Vector[A], A]) extends LossF[A] {
  def forward(y: NetworkOutput[A]): Array[A] = {
    euclidean(y.arrayX, y.arrayY)
  }

  def backward(predicted: Array[A], target: Array[A]): Array[A] = {
    euclideanD(predicted, target)
  }
}