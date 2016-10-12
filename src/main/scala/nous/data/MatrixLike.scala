package nous.data

import spire.implicits._

trait MatrixLike[A] {
  def data: Vector[A]
  def r: Int
  def c: Int

  def at(row: Int, col: Int): A = data.apply(row * c + col)
}