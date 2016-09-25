package nous.linalg.internal

trait Container[F[_]] {
  def size: Int
  def canGrow: Boolean
}