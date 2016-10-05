package nous.network

import nous.network.layers._
import nous.network.optimizers.Optimizer

trait NeuralNetwork[A] { self =>
  def layers: List[HiddenLayer[A]]
  def loss: (Vector[A], Vector[A]) => A
  def optimizer: Optimizer
}