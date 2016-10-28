package nous.data

import nous.data.vector.DenseVector

package object matrix {
  type Row[A] = DenseVector[A]
  type Col[A] = DenseVector[A]

  sealed trait Slice
  object :: extends Slice

  sealed trait StorageLayout
  case object RowMajor extends StorageLayout
  case object ColumnMajor extends StorageLayout

}