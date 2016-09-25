package nous.linalg
package internal

trait ElementTag[A] {

  def isValidScalar: Boolean

  def gpuCompatible: Boolean

  def bitSize: Int

  def makeVector(arr: Array[A]): Vector[A]

}

object ElementTag {

  def apply[A](implicit ev: ElementTag[A]): ElementTag[A] = ev

}