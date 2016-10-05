package nous

import cats._
import fs2._
import nous.data._
import nous.network.layers._
import nous.network.optimizers._

trait Model[F[_, _]] {
  //def layers[A]: List[Layer[A]]
  def inputs[A, B]: List[F[A, B]]
  def loss[G[_], H[_], A]: G[A] => H[A]
}
