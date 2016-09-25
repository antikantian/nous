package nous.linalg

package object internal {

  type A[I, A] = NArray[I, A]

  case class NArrayError(message: String) extends Exception

}