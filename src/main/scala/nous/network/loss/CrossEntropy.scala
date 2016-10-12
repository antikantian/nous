package nous.network
package loss

import scala.reflect.ClassTag

import nous.kernels.loss.{crossEntropy, crossEntropyD}
import spire.algebra._

final case class CrossEntropy[A: Field: Trig: ClassTag]() extends LossF[A] {
  def forward(y: NetworkOutput[A]): Vector[A] = {
    crossEntropy(y.vectorX, y.vectorY)
  }

  def backward(y: Vector[A], p: Vector[A]): Vector[A] = {
    crossEntropyD(y, p)
  }
}