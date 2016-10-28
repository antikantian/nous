package nous.data
package matrix

import scala.collection.mutable.ArrayBuffer

import spire.algebra._

abstract class Matrix[A] extends Product with Serializable {
  def rows: Int

  def cols: Int

  def size: Int = rows * cols

  def isTransposed: Boolean

  def isSquare: Boolean = rows == cols

  def apply(i: Int): A

  def apply(i: (Int, Int)): A

  def apply(i: Int, j: Int): A

  def index(i: Int): Int

  def index(i: Int, j: Int): Int

  def map[B](f: A => B): Matrix[B]

  def cmap[B](f: Col[A] => Col[B]): Matrix[B]

  def rmap[B](f: Row[A] => Row[B]): Matrix[B]

  def update(f: A => A): Matrix[A]

  def update(i: Int, j: Int, v: A): Matrix[A]

  def updateInto(a: ArrayBuffer[A]): Unit

  def transpose: Matrix[A]

  def iterator: Iterator[A]

  def toArray: Array[A]

}

object Matrix {
  //def zeros[A](rows: Int, cols: Int) = ???

  //def fill[A](rows: Int, cols: Int)(e: => A): Matrix[A] = ???

}