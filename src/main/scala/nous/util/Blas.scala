package nous.util

import cats.Eval
import com.github.fommil.netlib.BLAS
import nous.kernels.matrix.gemm_ref

trait Blas[A] {
  def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: A, a: Array[A], b: Array[A], beta: A): Array[A]

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
  }

  implicit def doubleBlas: Blas[Double] = new Blas[Double] {
    override def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: Double, a: Array[Double], b: Array[Double], beta: Double): Array[Double] = {
      val outArray = new Array[Double](m * n)
      val lda = getLda(transA, m, k)
      val ldb = getLdb(transB, n, k)
      nblas.value.dgemm(transA, transB, m, n, k, alpha, a, lda, b, ldb, beta, outArray, m)
      outArray
    }
  }

  implicit def intBlas: Blas[Int] = new Blas[Int] {
    override def gemm(transA: String, transB: String, m: Int, n: Int, k: Int, alpha: Int, a: Array[Int], b: Array[Int], beta: Int): Array[Int] = {
      val outArray = new Array[Int](m * n)
      val lda = getLda(transA, m, k)
      val ldb = getLdb(transB, n, k)
      gemm_ref(transA, transB, m, n, k, alpha, a, b, beta, outArray)
      outArray
    }
  }

}