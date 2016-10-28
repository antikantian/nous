package nous.kernels

import cats.Eval
import com.github.fommil.netlib.BLAS

trait Blas[A] {
  def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: A, a: Array[A], b: Array[A], beta: A): Array[A]
  def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: A, a: Array[A], lda: Int, b: Array[A], ldb: Int, beta: A, ldc: Int): Array[A]

  def getLda(transA: String, m: Int, k: Int) =
    if (transA == "N" || transA == "n") k else m

  def getLdb(transB: String, n: Int, k: Int) =
    if (transB == "N" || transB == "n") n else k
}

object Blas {

  val nblas = Eval.later(BLAS.getInstance())

  implicit def floatBlas: Blas[Float] = new Blas[Float] {
    override def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: Float, a: Array[Float], b: Array[Float], beta: Float): Array[Float] = {
      val outArray = new Array[Float](m * n)
      val lda = getLda(transA, m, k)
      val ldb = getLdb(transB, n, k)
      nblas.value.sgemm(transA, transB, m, n, k, alpha, a, lda, b, ldb, beta, outArray, m)
      outArray
    }

    override def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: Float, a: Array[Float], lda: Int, b: Array[Float], ldb: Int, beta: Float, ldc: Int): Array[Float] = {
      val outArray = new Array[Float](m * n)
      nblas.value.sgemm(transA, transB, m, n, k, alpha, a, lda, b, ldb, beta, outArray, ldc)
      outArray
    }
  }

  implicit def doubleBlas: Blas[Double] = new Blas[Double] {
    override def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: Double, a: Array[Double], b: Array[Double], beta: Double): Array[Double] = {
      val outArray = new Array[Double](m * n)
      val lda = getLda(transA, m, k)
      val ldb = getLdb(transB, n, k)
      nblas.value.dgemm(transA, transB, m, n, k, alpha, a, lda, b, ldb, beta, outArray, m)
      outArray
    }

    override def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: Double, a: Array[Double], lda: Int, b: Array[Double], ldb: Int, beta: Double, ldc: Int): Array[Double] = {
      val outArray = new Array[Double](m * n)
      nblas.value.dgemm(transA, transB, m, n, k, alpha, a, lda, b, ldb, beta, outArray, ldc)
      outArray
    }
  }

}