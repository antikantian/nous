package nous.network.layers

import scala.language.higherKinds

import nous.data._
import nous.network._

trait FunctionLayer[A] {
  def layerID: Int
  def layerLabel: String
  def layerName: String = s"$layerLabel-$layerID"
  def definition: LayerDefinition[A]
  def W: Weights[A]
  def bias: Vector[A]
  def inputShape: InputShape
  def outputShape: OutputShape
  def forward(x: Sample[A, A]): Sample[A, A]
  def backward(gradient: Vector[A], xa: Vector[A]): Vector[A]
  def updateW(weights: Vector[A]): FunctionLayer[A]
  def teachable = W.nonEmpty
  def renumber(n: Int): FunctionLayer[A]
  def a = definition.activation
}