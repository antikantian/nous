package nous

import cats.data._
import nous.data.Batch

package object network {
  type NetworkOutput[A] = Batch[A, A]

  case class Error[A](et: A, eo: Vector[A])

}