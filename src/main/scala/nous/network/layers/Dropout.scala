package nous.network
package layers

import scala.reflect.ClassTag

import nous.data._
import nous.network.definitions._
import nous.util.Shape
import spire.algebra._
import spire.math._
import spire.random._

class Dropout[A: ClassTag](dodef: DropoutDefinition[A], in: InputShape, id: Int = -1)(
    implicit val field: Field[A]) extends FunctionLayer[A] { self =>

  val layerID = id
  val layerLabel = "dropout"
  val definition = dodef
  val droprate = definition.r
  val inputShape = in
  val outputShape = inputShape

  lazy val W = Weights.empty[A]
  lazy val bias = Vector.empty[A]

  def renumber(n: Int): Dropout[A] = new Dropout[A](definition, inputShape, n)

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
    W.data
  }

  def updateW(weights: Vector[A]): FunctionLayer[A] = {
    self
  }

}