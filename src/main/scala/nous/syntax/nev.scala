package nous.syntax

import cats.data.NonEmptyVector

object nev {

  implicit class NonEmptyVectorOps[A](neva: NonEmptyVector[A]) {
    def scanl1(f: (A, A) => A): NonEmptyVector[A] = {
      val v = neva.toVector
      NonEmptyVector.fromVectorUnsafe(v.scanLeft(v.head)(f))
    }


  }

}