package nous.layers

import spire.math.Numeric

case class ReLU[A](implicit ev: Numeric[A]) extends ComputationLayer[A] {

  def setup(net: Network[A]): Unit = { }

  def forward(input: Matrix[A], output: Matrix[A]): Matrix[A]

  def backward(input: Matrix[A], gradient: Matrix[A], weights: Matrix[A]): Matrix[A]

}