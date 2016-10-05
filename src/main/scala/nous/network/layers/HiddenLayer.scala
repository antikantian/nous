package nous.network.layers

import scala.language.higherKinds

import cats._
import cats.data._
import fs2._
import nous.data._
import nous.util.Blas
import definitions._
import spire.algebra._

trait HiddenLayer[A] {
  def weights: Vector[A]
  def bias: Vector[A]
  def activation(fx: Vector[A]): Vector[A]
  def forward(x: LayerInput[A])(implicit ev: Field[A]): LayerOutput[A]
  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A]
  def updateW(weights: Vector[A]): HiddenLayer[A]
}