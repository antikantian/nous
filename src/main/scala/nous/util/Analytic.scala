package nous.util

trait Analytic[A] {
  def fromDouble(x: Double): A
  def fromFloat(x: Float): A
}

sealed abstract class AnalyticInstances {
  implicit val analyticDouble: Analytic[Double] =
    new Analytic[Double] {
      def fromDouble(x: Double) = identity(x)
      def fromFloat(x: Float) = identity(x.toDouble)
    }

  implicit val analyticFloat: Analytic[Float] =
    new Analytic[Float] {
      def fromDouble(x: Double) = identity(x.toFloat)
      def fromFloat(x: Float) = identity(x)
    }

  implicit def analyticReverseDouble: Analytic[]
}