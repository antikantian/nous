package nous

import cats._
import cats.arrow._

trait PFunction[S[_], T[_]] {
  def runPF[A, B](at: A => T[S], ts: T[S]): B
}

