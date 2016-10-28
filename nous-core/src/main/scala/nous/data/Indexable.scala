package nous.data

import language.experimental.macros

import dogs.{ Vector => DVector }
import imp.summon
import nous.data.vector.DenseVector

trait Indexable[F[_]] {
  def apply[A](idx: Int, fa: F[A]): A
  def length[A](fa: F[A]): Int
}

object Indexable {
  def apply[F[_]: Indexable]: Indexable[F] = macro summon[Indexable[F]]

  implicit val IndexableDenseVector: Indexable[DenseVector] =
    new Indexable[DenseVector] {
      def apply[A](idx: Int, fa: DenseVector[A]): A = fa.apply(idx)
      def length[A](fa: DenseVector[A]): Int = fa.length
    }

  implicit val IndexableDVector: Indexable[DVector] =
    new Indexable[DVector] {
      def apply[A](idx: Int, fa: DVector[A]): A = fa.apply(idx)
      def length[A](fa: DVector[A]): Int = fa.length
    }

}