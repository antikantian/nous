package nous.network

import cats.data._
import fs2._
import nous.data._

package object layers {
  type WeightInit[A] = Stream[Task, A]
  type LayerInput[A] = Batch[A, A]
  type LayerOutput[A] = Batch[A, A]
  type GradientOutput[A] = Vector[A]
  type InputShape = Shape
  type OutputShape = Shape
  type LambdaInjection[A] = (LayerInput[A] => LayerOutput[A], LayerOutput[A] => Vector[A])

  case class Shape(s: Int, k: Int, m: Int, n: Int) {
    def is1d = {
      s == 1 && k == 1 && m == 1 && n > 1
    }

    def is2d = {
      s == 1 && k == 1 && m > 1 && is1d
    }

    def is3d = {
      s == 1 && k > 1 && is2d
    }

    def is4d = s > 1 && is3d

  }

}