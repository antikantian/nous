package nous.syntax.ops

import nous.linalg._
import spire.algebra.Eq

final class MatrixOps[A](lhs: Matrix[A])(implicit eq: Eq[A]) {
  // Matrix-scalar comparisons
  def <(rhs: A): Matrix[Int]
  def <=(rhs: A): Matrix[Int]
  def >(rhs: A): Matrix[Int]
  def >=(rhs: A): Matrix[Int]
  def ==(rhs: A): Matrix[Int]
  def !=(rhs: A): Matrix[Int]

  // Matrix-scalar ops
  def *(rhs: A): Matrix[A]
  def +(rhs: A): Matrix[A]

  // Matrix-matrix ops
  def *(rhs: Matrix[A]): Matrix[A]
  def +(rhs: Matrix[A]): Matrix[A]
}