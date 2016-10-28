package nous

import scala.collection.mutable

import fs2.{Stream, Task}
import nous.data.Batch
import nous.util.Shape

package object network {
  type NetworkInput[A] = Batch[A, A]
  type NetworkOutput[A] = Batch[A, A]
  type CachedNetworkOutput[A] = (mutable.HashMap[String, mutable.IndexedSeqView[A, Array[A]]], Batch[A, A])
  type WeightInit[A] = Stream[Task, A]
  type InputShape = Shape
  type OutputShape = Shape
  type LambdaInjection[A] = (NetworkInput[A] => Batch[A, A], Batch[A, A] => Array[A])

}