package nous.network

import scala.collection.mutable
import scala.reflect.ClassTag

import cats._
import fs2._
import nous.data._
import nous.network.layers._
import nous.network.loss._
import nous.network.optimizers._
import nous.util.Shape
import spire.algebra.{Field, Module}
import spire.math._

trait NeuralNetwork[A] {

  // TODO: Dropout should only be used during training, not testing or at runtime.
  // There's probably an easy way to implement this using pattern matching or fs2

  implicit def dim = JetDim(3)

  def layers: List[FunctionLayer[A]]

  def lossF: LossF[A]

  def optimizer: Option[Optimizer[A]]

  def totalWeights: Int = layers.foldLeft(0) { (acc, layer) => acc + layer.W.length }

  def collectWeights: Vector[A] =
    Stream.emits(layers).fold(Vector.empty[A])((acc, layer) => acc ++ layer.W.data).toVector.head

  def layerWeights: Vector[Weights[A]] =
    layers.foldLeft(Vector.empty[Weights[A]]) { (acc, layer) => acc :+ layer.W }

  def unactivated(x: NetworkInput[A])(implicit ev: Field[A], ev1: ClassTag[A]): NetworkOutput[A] =
    layers.foldLeft(x) { (xbatch, layer) =>
      xbatch map { xsample =>
        layer.forward(xsample)
      }
    }

  def prop(x: NetworkInput[A])(implicit ev: Field[A], ev2: ClassTag[A]): CachedNetworkOutput[A] = {
    val ycached = Eval.now(mutable.HashMap.newBuilder[String, Vector[A]]) map { cache =>
      val y = layers.foldLeft(x) { (xbatch, layer) =>
        xbatch map { xsample =>
          val layerOutput = layer.forward(xsample)
          cache += (layer.layerName -> layerOutput.data)
          layerOutput
        }
      }
      (cache.result, y)
    }
    ycached.value
  }

  def propagate(x: NetworkInput[A])(implicit ev: Field[A], ev2: ClassTag[A]): NetworkOutput[A] =
    layers.foldLeft(x) { (xbatch, layer) =>
      xbatch map { xsample =>
        layer.a.forward(layer.forward(xsample))
      }
    }

  def deltaOut(batch: NetworkOutput[A])(implicit m: Module[Vector[A], A]): Vector[A] = {
    lossF.backward(batch.vectorX, batch.vectorY)
  }

  def backpropagate(y: CachedNetworkOutput[A])(implicit ev: ClassTag[A], ev2: Module[Vector[A], A]) = {
    Eval.now(y._1) map { cache =>
      val output = y._2
      val dout = deltaOut(output)
      cache foreach { kv =>
        val (k, v) = kv
        println(s"$k -> ${v.length}")
      }
      println(s"Network output size: ${output.data.head.data.length}")
      println(s"Delta output size: ${dout.length}")
      layers.reverse.tail.foldLeft(dout) { (gradient, layer) =>
        val activatedInput = cache.get(s"${layer.layerName}")
        println(s"${layer.layerName}: weights = ${layer.W.length}, gradient = ${gradient.length}")
        layer.backward(gradient, activatedInput.get)
      }

      /**
      layers.foldRight(dout) { (layer, gradient) =>
        //val cachedOutput = cache.get(layer.layerName)
        val activatedInput = cache.get(s"${layer.layerName}-a")
        println(s"${layer.layerName}: weights = ${layer.W.length}, gradient = ${gradient.length}")
        layer.backward(gradient, activatedInput.get)
      }
      */
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
