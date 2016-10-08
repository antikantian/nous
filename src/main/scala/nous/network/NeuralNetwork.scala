package nous.network

import cats._
import fs2._
import nous.network.definitions._
import nous.network.layers._
import nous.network.optimizers.Optimizer
import spire.algebra.Field
import spire.math._
import spire.implicits._

trait NeuralNetwork[A] {

  // TODO: Dropout should only be used during training, not testing or at runtime.
  // There's probably an easy way to implement this using pattern matching or fs2

  implicit def dim = JetDim(3)

  def layers: List[FunctionLayer[A]]

  def lossF: LossF[A]

  def collectWeights: Vector[A] =
    Stream.emits(layers).fold(Vector.empty[A])((acc, layer) => acc ++ layer.weights).toVector.head

  def layerWeights: Vector[LayerWeights[A]] =
    layers.foldLeft(Vector.empty[LayerWeights[A]]) { (acc, layer) => acc :+ layer.weights }

  def propagate(x: LayerInput[A])(implicit ev: Field[A]): LayerOutput[A] =
    Stream.emits(layers).fold(x)((input, layer) => layer.forward(input)).toVector.head

}

object NeuralNetwork extends NeuralNetworkInstances {

  def build[A](definitions: List[LayerDefinition[A]], input: Shape, loss: LossF[A]) =
    new NeuralNetwork[A] {
      val layers: List[FunctionLayer[A]] = {
        definitions.foldLeft(List.empty[FunctionLayer[A]]) { (acc, ldef) =>
          acc :+ ldef.build(input)
        }
      }
      val lossF: LossF[A] = loss
    }

}

abstract sealed class NeuralNetworkInstances {
  implicit def neuralNetworkSemigroup[A]: Semigroup[NeuralNetwork[A]] =
    new Semigroup[NeuralNetwork[A]] {
      def combine(x: NeuralNetwork[A], y: NeuralNetwork[A]): NeuralNetwork[A] =
        new NeuralNetwork[A] {
          val layers: List[FunctionLayer[A]] = x.layers ++ y.layers
          val lossF: LossF[A] = x.lossF
        }
    }
}
