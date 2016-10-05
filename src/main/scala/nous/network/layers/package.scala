package nous.network

import cats.data._
import fs2._
import nous.data._

package object layers {
  type Activation[A] = A => A
  type WeightInit[A] = Stream[Task, A]
  type LayerInput[A] = Batch[A, A]
  type LayerOutput[A] = Batch[A, A]
  type GradientOutput[A] = Vector[A]
  type InputShape = Shape
  type OutputShape = Shape
  type LambdaInjection[A] = (LayerInput[A] => LayerOutput[A], LayerOutput[A] => Vector[A])

  sealed trait Shape
  case class Shape3D(samples: Int, timesteps: Int, inputs: Int) extends Shape
  case class Shape4D(samples: Int, channels: Int, height: Int, width: Int) extends Shape

}