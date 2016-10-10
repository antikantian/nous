package nous.network.layers

import scala.language.higherKinds

import nous.data._
import nous.network._
import nous.network.definitions.LayerDefinition

trait FunctionLayer[A] {
  def layerID: Int
  def layerLabel: String
  def definition: LayerDefinition[A]
  def W: Weights[A]
  def bias: Vector[A]
  def inputShape: InputShape
  def outputShape: OutputShape
  def forward(x: LayerInput[A]): LayerOutput[A]
  def backward(y: NetworkOutput[A], g: Vector[A]): Vector[A]
  def updateW(weights: Vector[A]): FunctionLayer[A]
  def teachable = W.nonEmpty
  def renumber(n: Int): FunctionLayer[A]
  def a = definition.activation
}