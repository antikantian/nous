package nous.network
package loss

import scala.reflect.ClassTag

import fs2._
import nous.network.layers._
import spire.algebra._
import spire.implicits._

final case class MeanSquaredError[A: Field: ClassTag](implicit m: Module[Vector[A], A]) extends LossF[A] {
  def f(o: NetworkOutput[A]): Error[A] = {
    val y = o.sampleStream.flatMap(sample => Stream.emits(sample.data)).toVector
    val t = o.sampleStream.flatMap(sample => Stream.emits(sample.target)).toVector
    assert(y.length == t.length, "Network prediction length does't match target length")
    val e = m.minus(y, t)
    Error(e.dot(e) / e.length, e)
  }

  // Compute the derivative with respect to weights
  // Need (X, y)
  // Have y' == f(X)
  // Steps:
  // (1) deltaN = elt-wise multiplication: (predictions - targets) * f'(zN) (f'(zN) == activation output of last layer)
  // (2) dJdW(N-1) = gemm(activation_output(N-1), deltaN)
  // (3) delta(N-1) = gemm(deltaN, W(N-1).transpose) * f'(zN-1)
  // (4) dJdW(N-2) = gemm(X.transpose, deltaN-1) (assuming this reaches the first layer)
  // (yield) (dJdW(N-2), dJdW(N-1))
  def df(o: NetworkOutput[A]): Vector[A] = {
    /**
    val y = o.sampleStream.flatMap(sample => Stream.emits(sample.data)).toVector
    val t = o.sampleStream.flatMap(sample => Stream.emits(sample.target)).toVector
    assert(y.length == t.length, "Network prediction length doesn't match target length")
    */
    Vector.empty[A]
  }
}