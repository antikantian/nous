package nous.network.loss

import nous.network.layers.LayerInput

trait LossLayer[A] {
  def calculate(predicted: Vector[A], target: Vector[A]): Vector[A]
}