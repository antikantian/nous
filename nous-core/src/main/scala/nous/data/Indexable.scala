package nous.data

import language.experimental.macros

import imp.summon

trait Indexable[F[_]] {
  def apply[A](fa: F[A], idx: Int): A
  def get[A](fa: F[A], idx: Int): Option[A]
  def length[A](fa: F[A]): Int
  def toArray[A](fa: F[A]): Array[A]
}

object Indexable {
  def apply[F[_]: Indexable]: Indexable[F] = macro summon[Indexable[F]]
}