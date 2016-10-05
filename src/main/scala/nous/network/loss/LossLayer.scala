package nous.network.loss

trait LossLayer[A] {
  def compute(y: Vector[A], targets: Vector[A]): A
}