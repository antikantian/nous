package nous.layers

import scala.language.higherKinds

import cats._
import cats.data._
import fs2._
import nous.data._
//import nous.free.tensor.TensorOp

/**

trait Layer[A] {
  def label: String
  def compiler: TensorOp[A] ~> Id
  def bias: Matrix[A]
  def weights: Tensor[A]
  def computeInplace: Boolean
  def forward(input: Tensor[A]): Task[Unit] Xor Task[Tensor[A]]
  def backward(gradInput: Tensor[A], gradWeights: Tensor[A]): Task[Unit] Xor Task[Tensor[A]]
}

*/
