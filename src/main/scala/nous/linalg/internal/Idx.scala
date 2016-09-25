package nous.linalg.internal

import spire.algebra.Eq

trait Idx[I] {
  def idxType: I
  def dim: Int
  def name: String
}

sealed abstract class IdxInstances {

  implicit def idxCanEq[I]: Eq[Idx[I]] = new Eq[Idx[I]] {
    def eqv(x: Idx[I], y: Idx[I]): Boolean = x.name == y.name
  }

}