package nous.network.layers

import scala.language.higherKinds

import nous.kernels.Blas
import nous.network.definitions.LayerDefinition
import spire.algebra._
import spire.math._

trait FunctionLayer[A] {
  implicit def field: Field[A]

  def definition: LayerDefinition[A]
  def weights: Vector[A]
  def bias: Vector[A]
  def inputShape: Shape
  def outputShape: Shape
  def forward(x: LayerInput[A]): LayerOutput[A]
  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A]
  def updateW(weights: Vector[A]): FunctionLayer[A]

  //private[nous] def one(implicit field: Field[A]) = field.one
  //private[nous] def zero(implicit field: Field[A]) = field.zero
}