package nous.linalg

import nous.linalg.internal.ElementTag

sealed abstract class Scalar[+A] { self =>
  def isSNil: Boolean
  def get: A

  @inline final def map[B](f: A => B): Scalar[B] =
    if (isSNil) SNil else SVal(f(get))

  @inline final def flatMap[B](f: A => Scalar[B]): Scalar[B] =
    if (isSNil) SNil else f(self.get)

  @inline final def foreach(f: A => Unit) =
    if (!isSNil) f(self.get)
}

case class SVal[+A](e: A) extends Scalar[A] {
  def isSNil = Predef.implicitly[ElementTag[A]].isValidScalar
  def get = e

  override def toString = e.toString
}

case object SNil extends Scalar[Nothing] {
  def isSNil = true
  def get = throw new NoSuchElementException("SNil.get")

  override def toString = "SNil"
}