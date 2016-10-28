package nous.data
package vector

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

import spire.algebra._

sealed class DenseVector[A: ClassTag](elems: Array[A]) {

  def apply(idx: Int) = elems(idx)

  def length: Int = size

  def map[B: ClassTag](f: A => B): DenseVector[B] =
    new DenseVector(elems.map(f))

  def update(idx: Int, v: A): Unit = elems.update(idx, v)

  def iterator = elems.view.iterator

  def size: Int = elems.length

}

object DenseVector {
  def apply[A: ClassTag](elems: Seq[A]): DenseVector[A] = new DenseVector(elems.toArray)
}
