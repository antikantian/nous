package nous.data
package matrix

import scala.collection.{GenIterable, mutable}
import scala.collection.mutable.ArrayBuffer

import cats.Eval
import dogs.{Vector => DVector}
import nous.data.vector._
import shapeless.syntax.std.tuple._
import spire.algebra._
import spire.implicits._

final case class DenseMatrix[A](private[nous] val vectors: DVector[DenseVector[A]])
  extends Matrix[A] {

  private[nous] var _transposed: Boolean = false
  private[nous] val _r = vectors.length
  private[nous] val _c = vectors(0).length

  def isTransposed: Boolean = _transposed

  def rows: Int =
    if (!isTransposed) _r else _c

  def cols: Int =
    if (!isTransposed) _c else _r

  def apply(i: Int, j: Int): A = vectors(i)(j)

  def map[B](f: A => B): DenseMatrix[B] = {
    val vecs = iterator.map(f).grouped(_c).map(g => DenseVector(g.toArray))
    val result = vecs.foldLeft(DVector.empty[DenseVector[B]]) { _ :+ _ }
    DenseMatrix(result)
  }

  def cmap[B](f: Col[A] => Col[B]): DenseMatrix[B] = {
    val cb = colIterator map f
    val result = cb.foldLeft(DVector.empty[Col[B]]) { _ :+ _ }
    DenseMatrix(result)
  }

  def rmap[B](f: Row[A] => Row[B]): DenseMatrix[B] = {
    val rb = rowIterator map f
    val result = rb.foldLeft(DVector.empty[Row[B]]) { _ :+ _ }
    DenseMatrix(result)
  }

  def update(f: A => A): Unit = {
    for (i <- (0 until rows).par; j <- (0 until cols).par) {
      vectors(i)(j) = f(vectors(i)(j))
    }
  }

  def update(i: Int, j: Int, v: A): Unit = vectors(i).update(j, v)

  //def transpose: DenseMatrix[A] = new DenseMatrix(r, c, elems.transpose, true)

  def T(f: DenseMatrix[A] => Unit): Unit = {
    val tmpT = DenseMatrix(vectors)
    tmpT._transposed = true
    f(tmpT)
  }

  def iterator: Iterator[A] =
    new Iterator[A] {
      private[nous] var rowcursor: Int = 0
      private[nous] var colcursor: Iterator[A] = vectors(0).iterator

      def hasNext: Boolean = colcursor.hasNext || rowcursor < rows

      def next: A =
        if (colcursor.hasNext)
          colcursor.next
        else if (!colcursor.hasNext && rowcursor < rows) {
          colcursor = vectors(rowcursor).iterator
          rowcursor += 1
          colcursor.next
        }
        else
          throw new NoSuchElementException("Empty iterator has no next")
    }

  def colIterator: Iterator[Col[A]] =
    new Iterator[Col[A]] {
      val cidx = colIndex.iterator.grouped(rows)

      def hasNext: Boolean = cidx.hasNext

      def next: DenseVector[A] = {
        DenseVector(cidx.next.map(apply).toArray)
      }

    }

  def rowIterator: Iterator[Row[A]] =
    new Iterator[Row[A]] {
      private[nous] var rowcursor = 0

      def hasNext: Boolean = rowcursor < rows

      def next: Row[A] = {
        if (hasNext) {
          rowcursor += 1
          vectors(rowcursor)
        }
        else
          throw new NoSuchElementException("Empty iterator has no next")
      }
    }

  def copyToArray: Array[A] = {
    val result = new Array[A](rows * cols)
    val cvecs = vectors.zipWithIndex.toScalaVector.par

    cvecs foreach { r =>
      val (cvec, rownum) = r
      cfor(0)(_ < cvec.length, _ + 1) { cidx =>
        val linearPosition = rownum * _c + cidx
        result.update(linearPosition, cvec(cidx))
      }
    }
    result
  }

  private[nous] def colIndex: IndexedSeq[(Int, Int)] = {
    val buffer = new ArrayBuffer[(Int, Int)](rows * cols)
    cfor(0)(_ < cols, _ + 1) { c =>
      cfor(0)(_ < rows, _ + 1) { r =>
        buffer += ((r, c))
      }
    }
    buffer.toIndexedSeq
  }

  private[nous] def rowIndex: IndexedSeq[(Int, Int)] = {
    val buffer = new ArrayBuffer[(Int, Int)](rows * cols)
    cfor(0)(_ < rows, _ + 1) { r =>
      cfor(0)(_ < cols, _ + 1) { c =>
        buffer += ((r, c))
      }
    }
    buffer.toIndexedSeq
  }

}
