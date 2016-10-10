package nous.network

import fs2._
import nous.data._
import nous.util.Shape

package object layers {
  type WeightInit[A] = Stream[Task, A]
  type LayerInput[A] = Batch[A, A]
  type LayerOutput[A] = Batch[A, A]
  type GradientOutput[A] = Vector[A]
  type InputShape = Shape
  type OutputShape = Shape
  type LambdaInjection[A] = (LayerInput[A] => LayerOutput[A], LayerOutput[A] => Vector[A])

}