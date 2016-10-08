package nous

import cats.data._
import nous.data.Batch

package object network {
  type ActivationF[A] = A => A
  type LossF[A] = (Vector[A], Vector[A]) => A
  type LayerWeights[A] = Vector[A]
  type NetworkOutput[A] = Vector[A]
  type ForwardOutput[A] = Batch[A, A]

}