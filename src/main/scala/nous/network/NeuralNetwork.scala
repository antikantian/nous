package nous.network

import nous.network.definitions._
import nous.network.layers._
import nous.network.optimizers.Optimizer
import spire.algebra._
import spire.math._

trait NeuralNetwork[A] {

  def layers: List[HiddenLayer[A]]

  def loss: Vector[A] => A

  def propagate(x: LayerInput[A])(implicit ev: Field[A]): LayerOutput[A] =
    layers.foldLeft(x) { (input, layer) => layer.forward(input) }
}

object NeuralNetwork {

  def build[A: Numeric](definitions: List[LayerDefinition[A]], input: Shape, loss: ) =
    new NeuralNetwork[A] {
      val layers: List[HiddenLayer[A]] = {
        definitions.foldLeft(List.empty[HiddenLayer[A]]) { (acc, ldef) =>
          acc :+ ldef.build(input)
        }
      }
    }

}
