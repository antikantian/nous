package nous.network
package layers

import scala.reflect.ClassTag

import nous.kernels.Blas
import nous.network.definitions._
import spire.algebra._
import spire.math._
import spire.implicits._


class Softmax[A: ClassTag](sdef: SoftmaxDefinition[A], input: Shape)(
    implicit
    val field: Field[A],
    val vs: NormedVectorSpace[Vector[A], A],
    val ord: Order[A],
    val trig: Trig[A],
    val nt: NumberTag[A])
  extends FunctionLayer[A] { self =>

  val definition = sdef
  val weights = Vector.empty[A]
  val bias = weights

  val inputShape = input
  val outputShape = inputShape

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x map { sample =>
      val snorm = sample mapChannels { channel =>
        val cnorm = nt.hasNegativeInfinity.map { inf =>
          channel.data.map { a =>
            val ma = max(inf, a)
            trig.exp(a - ma)
          }
        } match {
          case Some(v) => v.normalize
          case _ => channel.data.normalize
        }
        channel.update(cnorm)
      }
      snorm
    }
  }

  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A] = {
    weights
  }

  def updateW(weights: Vector[A]): FunctionLayer[A] = {
    self
  }

}