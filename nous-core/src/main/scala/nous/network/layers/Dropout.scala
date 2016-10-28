package nous.network
package layers

import scala.reflect.ClassTag

import nous.data._
import spire.algebra._
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
  lazy val bias = Array.empty[A]

  def renumber(n: Int): Dropout[A] = new Dropout[A](definition, inputShape, n)

  def forward(x: Sample[A, A]): Sample[A, A] = {
    val mask =
      Uniform[Float](0, 1)
        .map(n => if (n >= droprate) true else false)
        .pack(x.data.length)
        .apply(Generator.rng)

    val xout = x.data.zip(mask) map { a => if (a._2) a._1 else field.zero }
    x.update(x.channels, x.height, x.width, xout)

  }

  def backward(gradient: Array[A], xa: Array[A]): Array[A] = {
    gradient
  }

  def updateW(weights: Array[A]): FunctionLayer[A] = {
    self
  }

}