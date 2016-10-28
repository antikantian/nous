package nous.util

import cats._
import cats.data._
import cats.data.Validated.{ Invalid, Valid }

object validation {

  def parValidate[E: Semigroup, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
    (v1, v2) match {
      case (Valid(a), Valid(b)) => Valid(f(a, b))
      case (Valid(_), i@Invalid(_)) => i
      case (i@Invalid(_), Valid(_)) => i
      case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
    }

}