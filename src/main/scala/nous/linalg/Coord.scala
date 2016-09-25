package nous.linalg

import spire.algebra.Order

sealed trait Coord {
  def x: Int
  def y: Int
}

object Coord {
  case class Point(x: Int, y: Int) extends Coord
  case class PointWithVal[A](x: Int, y: Int, a: A) extends Coord
  case class PointMaybeVal[A](x: Int, y: Int, a: Option[A]) extends Coord
}

sealed abstract class CoordInstances {
  import Coord._

  implicit def pointWithValIsOrdered[A]: Order[PointWithVal[A]] =
    new Order[PointWithVal[A]] {
      def compare(x: PointWithVal[A], y: PointWithVal[A]): Int =
        Predef.implicitly[Order[A]].compare(x.a, y.a)
    }

}


