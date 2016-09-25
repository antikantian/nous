package nous.linalg

trait Tensor[A] {
  /** Number of contravariant indices */
  def n: Int

  /** Number of covariant indices */
  def m: Int

  def order: Int = n + m
}