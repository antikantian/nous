package nous.data

import scala.collection.GenIterable

import nous.data.matrix.Row

trait IndexedData[A] {
  def apply(i: Int): A
  def contains[A1 >: A](elem: A1): Boolean
  def count(p: A => Boolean): Int
  def distinct: IndexedData[A]
  def drop(n: Int): IndexedData[A]
  def exists(p: A => Boolean): Boolean
  def filter(p: A => Boolean): IndexedData[A]
  def filterNot(p: A => Boolean): IndexedData[A]
  def find(p: A => Boolean): IndexedData[A]
  def flatMap[B](f: A => IndexedData[B]): IndexedData[B]
  def foldLeft[B](b: B)(f: (B, A) => B): B
  def foldRight[B](b: B)(f: (A, B) => B): B
  def forall(p: A => Boolean): Boolean
  def foreach(f: A => Unit): Unit
  def groupBy[K](f: A => K): Map[K, IndexedData[A]]
  def grouped(size: Int): Iterator[IndexedData[A]]
  def head: A
  def headOption: Option[A]
  def index(i: Int): Int
  def indices: Range
  def isEmpty: Boolean
  def iterator: Iterator[A]
  def last: A
  def lastOption: Option[A]
  def length: Int
  def map[B](f: A => B): IndexedData[B]
  def max: A
  def maxBy[B](f: A => B): A
  def min: A
  def minBy[B](f: A => B): A
  def nonEmpty: Boolean
  def product: A
  def reduceLeft[B >: A](f: (B, A) => B): B
  def reverse: IndexedData[A]
  def reverseIterator: Iterator[A]
  def scan1(f: (A, A) => A): IndexedData[A]
  def scanLeft[B, That](b: B)(f: (B, A) => B): That
  def size: Int
  def sliding(size: Int, step: Int): Iterator[IndexedData[A]]
  def sliding(size: Int): Iterator[IndexedData[A]]
  def sum: A
  def tail: IndexedData[A]
  def tails: Iterator[IndexedData[A]]
  def take(n: Int): IndexedData[A]
  def takeRight(n: Int): IndexedData[A]
  def takeWhile(p: A => Boolean): IndexedData[A]
  def to[Col[_]]: Col[A]
  def toArray: Array[A]
  def update(i: Int, v: A): Unit
  def update(f: A => A): Unit
  def zip[B](that: GenIterable[B]): IndexedData[(A, B)]
  def zip2[B, C](b: GenIterable[B], c: GenIterable[C]): IndexedData[(A, B, C)]
  def zip3[B, C, D](b: GenIterable[B], c: GenIterable[C], d: GenIterable[D]): IndexedData[(A, B, C, D)]
  def zipWith[B](that: GenIterable[B])(f: (B, A) => (A, B)): IndexedData[(A, B)]
  def zipWithIndex: IndexedData[(A, Int)]
}

trait IndexedData2D[I[_], J[_], A] extends IndexedData[A] {
  def apply(i: Int, j: Int): A
  def index(i: Int, j: Int): Int
  override def map[B](f: A => B): IndexedData2D[I, J, B]
  def imap[B](f: I[A] => I[B]): IndexedData2D[I, J, B]
  def jmap[B](f: J[A] => J[B]): IndexedData2D[I, J, B]
  def transpose: IndexedData2D[I, J, A]
  def update(i: Int, j: Int, v: A): Unit
}

trait IndexedData3D[I[_], J[_], K[_], A] extends IndexedData2D[I, J, A] {
  def apply(i: Int, j: Int, k: Int): A
  def index(i: Int, j: Int, k: Int): Int
  def kmap[B](f: K[A] => K[B]): IndexedData3D[I, J, K, B]
}

trait IndexedData4D[I[_], J[_], K[_], L[_], A] extends IndexedData3D[I, J, K, A] {
  def apply(i: Int, j: Int, k: Int, l: Int): A
  def index(i: Int, j: Int, k: Int, l: Int): Int
  def lmap[B](f: L[A] => L[B]): IndexedData4D[I, J, K, L, A]
}