package nous

import cats._

/** We'll call a layer a parameterized differentiable function
 *  F[Double] => G[Double]
 */
trait Computable[F[_], G[_], A] {

  def weights[T[_]]: T[A]

  def initR[T[_]]: T[A]

  def forward(fa: F[A]): G[A]

}