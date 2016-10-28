package nous.network
package loss

import scala.reflect.ClassTag

import nous.kernels.loss.{crossEntropy, crossEntropyD}
import spire.algebra._

final case class CrossEntropy[A: Field: Trig: ClassTag]() extends LossF[A] {
  def forward(y: NetworkOutput[A]): Array[A] = {
    crossEntropy(y.arrayX, y.arrayY)
  }

  def backward(prediction: Array[A], target: Array[A]): Array[A] = {
    crossEntropyD(prediction, target)
  }
}