package nous

import scala.collection.mutable

import fs2.{Stream, Task}
import nous.data.Batch
import nous.util.Shape

package object network {
  type NetworkInput[A] = Batch[A, A]
  type NetworkOutput[A] = Batch[A, A]
  type CachedNetworkOutput[A] = (mutable.HashMap[String, Vector[A]], Batch[A, A])
  type WeightInit[A] = Stream[Task, A]
  type GradientOutput[A] = Vector[A]
  type InputShape = Shape
  type OutputShape = Shape
  type LambdaInjection[A] = (NetworkInput[A] => Batch[A, A], Batch[A, A] => Vector[A])

  case class Error[A](et: A, eo: Vector[A])

}