package nous.data

import spire.algebra._
import spire.math._

sealed trait Coord {
  def x: Int
  def y: Int
}

object Coord {
  case class Point(x: Int, y: Int) extends Coord
  case class PointWithVal[A](x: Int, y: Int, a: A) extends Coord
  case class PointMaybeVal[A](x: Int, y: Int, a: Option[A]) extends Coord
  case class Im2Col(r: Int, c: Int, k: Int, y: Int, x: Int) extends Coord
}

sealed abstract class CoordInstances {
  import Coord._

  implicit def pointWithValIsOrdered[A](implicit ev: Numeric[A], ev1: Order[A]): Order[PointWithVal[A]] =
    new Order[PointWithVal[A]] {
      def compare(x: PointWithVal[A], y: PointWithVal[A]): Int =
        ev1.compare(x.a, y.a)
    }

}


