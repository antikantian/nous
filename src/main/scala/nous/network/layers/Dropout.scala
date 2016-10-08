package nous.network
package layers

import scala.reflect.ClassTag

import nous.kernels._
import nous.network.definitions._
import spire.algebra._
import spire.math._
import spire.random._

class Dropout[A: ClassTag](dodef: DropoutDefinition[A], input: Shape)(
    implicit val field: Field[A]) extends FunctionLayer[A] { self =>

  lazy val weights = Vector.empty[A]
  lazy val bias = weights

  val definition = dodef
  val droprate = definition.r
  val inputShape = input
  val outputShape = inputShape

  def forward(x: LayerInput[A]): LayerOutput[A] = {
    x map { sample =>
      val mask =
        Uniform[Float](0, 1)
          .map(n => if (n >= droprate) true else false)
          .pack(sample.data.length)
          .apply(Generator.rng)

      val y = sample.data.zip(mask) map { a => if (a._2) a._1 else field.zero }
      sample.update(sample.channels, sample.height, sample.width, y)
    }
  }

  def backward(x: LayerInput[A], yg: GradientOutput[A]): Vector[A] = {
    weights
  }

  def updateW(weights: Vector[A]): FunctionLayer[A] = {
    self
  }

}