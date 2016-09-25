package nous.linalg

trait ColumnVector[A] extends Matrix[A] {
  def diagm: Matrix[A]
}