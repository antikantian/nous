package nous.network

import cats._
import fs2._
import nous.data._
import nous.network.definitions._
import nous.network.layers._
import nous.network.loss._
import nous.network.optimizers._
import nous.util.Shape
import spire.algebra.{ Field, Rig }
import spire.math._
import spire.implicits._

trait NeuralNetwork[A] {

  // TODO: Dropout should only be used during training, not testing or at runtime.
  // There's probably an easy way to implement this using pattern matching or fs2

  implicit def dim = JetDim(3)

  def layers: List[FunctionLayer[A]]

  def lossF: LossF[A]

  def optimizer: Option[Optimizer[A]]

  def loss(y: NetworkOutput[A]): Error[A] = lossF.f(y)

  def totalWeights: Int = layers.foldLeft(0) { (acc, layer) => acc + layer.W.length }

  def collectWeights: Vector[A] =
    Stream.emits(layers).fold(Vector.empty[A])((acc, layer) => acc ++ layer.W.data).toVector.head

  def layerWeights: Vector[Weights[A]] =
    layers.foldLeft(Vector.empty[Weights[A]]) { (acc, layer) => acc :+ layer.W }

  def propagate(x: LayerInput[A])(implicit ev: Field[A]): LayerOutput[A] =
    layers.foldLeft(x) { (input, layer) =>
      layer.definition.activation.forward(layer.forward(input))
    }

  // After forwardprop, we have:
  // 1) Network output of last activation function, y = f_last(x): y = [a1, a2, a3, ..., an]
  // 2) Targets from the input batch, t = [t1, t2, t3, ..., tn]
  // 3) Loss over the mini-match, yt = [yt1, yt2, yt3, ..., ytn]
  // 4) Total error, A, some fractional
  // We need:
  // a) Derivative of last layer's activation function, which takes as input the network's output: f_last'(y) = [da1, da2, da3, ..., dan]
  // b) Delta_last, which is yt * f_last'(y) <-- this is the error that we're backpropagating
  // Take delta_last and fold right through the layers, feed the output of the final layer's activation function into
  // the backward's method of a layer, along with the accumulating delta/gradient
  // To backpropagate the error, each layer needs: backward(grad: Vector[A], y: NetworkOutput[A], aka LayerInput[A], aka LayerOutput[A])
  // This will give up the gradient with which we can update the weights, as Vector[A]
  // Going to store the loss (y - t) in g and initialize the fold with it
  def backpropagate(y: NetworkOutput[A], g: Vector[A]) = {
    assert(y.size == g.length, "Network output doesn't match size of output error")
    layers.foldRight(g) { (layer, d) =>
      val da = layer.a.backward(y, d)
      layer.backward(y, da)
    }
  }


}

object NeuralNetwork extends NeuralNetworkInstances {

  def build[A](definitions: List[LayerDefinition[A]], input: Shape, loss: LossF[A], opt: Option[Optimizer[A]] = None) =
    new NeuralNetwork[A] {
      val layers: List[FunctionLayer[A]] = {
        definitions.zipWithIndex.foldLeft(List.empty[FunctionLayer[A]]) { (acc, ldefn) =>
          val (ldef, idx) = ldefn
          acc :+ ldef.build(input).renumber(idx)
        }
      }
      val lossF: LossF[A] = loss
      val optimizer = opt
    }
}

abstract sealed class NeuralNetworkInstances {
  implicit def neuralNetworkSemigroup[A]: Semigroup[NeuralNetwork[A]] =
    new Semigroup[NeuralNetwork[A]] {
      def combine(x: NeuralNetwork[A], y: NeuralNetwork[A]): NeuralNetwork[A] =
        new NeuralNetwork[A] {
          val layers: List[FunctionLayer[A]] = (x.layers ++ y.layers).zipWithIndex.map { layern =>
            val (layer, n) = layern
            layer.renumber(n)
          }
          val lossF: LossF[A] = x.lossF
          val optimizer = x.optimizer
        }
    }
}
