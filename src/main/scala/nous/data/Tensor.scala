package nous.data

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.{Vector => SVector}
import scala.reflect.ClassTag

import breeze.linalg.DenseMatrix
import cats._
import debox.Buffer
import fs2._
import fs2.util.Attempt
import nous.util.exception._
import spire.math._
import spire.random._
import spire.implicits._

class Tensor[A](numSamples: Int, d: Int, nr: Int, nc: Int)(values: Buffer[A])(implicit ct: ClassTag[A]) { self =>
  val data = values
  val size = values.length
  val depth = d
  val rows = nr
  val cols = nc
  val sampleSize = depth * rows * cols
  val matrixSize = rows * cols
  val step = rows * cols
  val nsamples = numSamples
  val shape = (nsamples, depth, rows, cols)

  val samples = Eval.always {
    require(data.length % (depth * rows * cols) == 0, "Underlying array has irregular size.")
    data.length / (depth * rows * cols)
  }

  def raw(sample: Int, channel: Int, row: Int, col: Int): A =
    data.apply {
      ((sample * depth + channel) * rows + row) * cols + col
    }

  def indexOf(sample: Int, channel: Int, row: Int, col: Int): Int =
    ((sample * depth + channel) * rows + row) * cols + col

  def isEmpty: Boolean = nsamples == 0

  def nonEmpty: Boolean = nsamples > 0

  def dataIterator = data.iterator

  //def sampleIterator = self.toSamples.iterator

  def matrixIterator = self.toMatrices.iterator

  /** Concatenate two tensors, returning a new tensor */
  def ++(tensor: Tensor[A]): Tensor[A] = {
    val buf = self.data ++ tensor.data
    if (self.isEmpty)
      new Tensor(tensor.nsamples, tensor.depth, tensor.rows, tensor.cols)(buf)
    else if (tensor.isEmpty)
      new Tensor(self.nsamples, self.depth, self.rows, self.cols)(buf)
    else if (nonEmpty && tensor.nonEmpty && depth == tensor.depth && rows == tensor.rows && cols == tensor.cols)
      new Tensor(nsamples + tensor.nsamples, depth, rows, cols)(buf)
    else
      throw TensorConcatError("Cannot discern appropriate dimensionality for concatenation.")
  }

  def reshape(_samples: Int, _depth: Int, _rows: Int, _cols: Int): Tensor[A] =
    new Tensor(_samples, _depth, _rows, _cols)(data)

  def map[B: ClassTag: Numeric](f: A => B): Tensor[B] =
    new Tensor(nsamples, d, nr, nc)(data map f)

  def mapBreeze[B: ClassTag: Numeric](f: DenseMatrix[A] => DenseMatrix[B]): Tensor[B] = {
    Stream
      .emits(data.elems)
      .vectorChunkN(step)
      .map(vec => f(DenseMatrix.create(rows, cols, vec.toArray, 0, cols, isTranspose = true)))
      .map(mat => new Tensor(1, depth, mat.rows, mat.cols)(Buffer.unsafe(mat.data)))
      .fold(Tensor.empty[B])((acc, tensor) => acc ++ tensor)
      .toVector
      .head
  }

  def mapMatrix[B: ClassTag: Numeric](f: Matrix[A] => Matrix[B]): Tensor[B] = {
    Stream
      .emits(data.elems)
      .vectorChunkN(step)
      .map(vec => f(Matrix(rows, cols, vec.toArray)))
      .map(mat => new Tensor(1, depth, mat.rows, mat.cols)(Buffer.unsafe(mat.data)))
      .fold(Tensor.empty[B])((acc, tensor) => acc ++ tensor)
      .toVector
      .head
  }

  def mapSamples[B: ClassTag: Numeric](f: Sample[A] => Sample[B]): Tensor[B] = {
    Stream
      .emits(data.toVector)
      .vectorChunkN(sampleSize)
      .map(vec => f(Sample(depth, rows, cols, vec.toArray)))
      .map(sample => new Tensor(1, sample.depth, sample.rows, sample.cols)(Buffer.unsafe(sample.values)))
      .fold(Tensor.empty[B])((acc, tensor) => acc ++ tensor)
      .toVector
      .head
  }

  def foreachBreezeMatrix(f: DenseMatrix[A] => Unit): Attempt[Unit] = {
    Stream
      .emits(data.elems)
      .vectorChunkN(step)
      .map(vec => f(DenseMatrix.create(rows, cols, vec.toArray, 0, cols, isTranspose = true)))
      .run
  }

  def foreachMatrix(f: Matrix[A] => Unit): Attempt[Unit] = {
    Stream
      .emits(data.elems)
      .vectorChunkN(step)
      .map(vec => f(Matrix(rows, cols, vec.toArray)))
      .run
  }

  def foreachSample(f: Sample[A] => Unit): Attempt[Unit] = {
    Stream
      .emits(data.toVector)
      .vectorChunkN(sampleSize)
      .map(vec => f(Sample(depth, rows, cols, vec.toArray)))
      .run
  }

  def setSample(idx: Int, s: Sample[A]): Unit = {
    require(s.size == sampleSize, "Sample sizes must be equal")
    val startIdx = idx * sampleSize
    val sview = data.elems.view(startIdx, sampleSize)
    System.arraycopy(s.values, 0, sview, 0, sampleSize)
  }

  def setMatrix(idx: Int, m: Matrix[A]): Unit = {
    require(m.length == step, "Matrix sizes must be equal")
    val startIdx = idx * step
    val mview = data.elems.view(startIdx, step)
    System.arraycopy(m.data, 0, mview, 0, step)
  }

  def matrixAt(sample: Int, channel: Int): Matrix[A] = {
    val startIdx = indexOf(sample, channel, 0, 0)
    new Matrix(rows, cols)(data.elems.slice(startIdx, startIdx + (rows * cols)))
  }

  def insertSample(idx: Int, s: Sample[A]): Tensor[A] = {
    val startIdx = idx * sampleSize
    self.synchronized {
      values.splice(startIdx, s.values)
      new Tensor[A](numSamples + 1, d, nr, nc)(values)
    }
  }

  def toSamples: List[Sample[A]] =
    toStream
      .vectorChunkN(sampleSize)
      .zipWithIndex
      .map(vi => Sample(depth, rows, cols, vi._2, vi._1.toArray))
      .toList

  def toMatrices: List[Matrix[A]] =
    toStream
      .vectorChunkN(step)
      .map(vec => Matrix(rows, cols, vec.toArray))
      .toList

  def asMatrix: Matrix[A] =
    if (data.length != 0) {
      val numSamples = samples.value
      new Matrix(numSamples, data.length / numSamples)(data.toArray)
    } else {
      Matrix.empty[A]
    }

  def toStream: Stream[Pure, A] = Stream.emits(data.elems).pure

  override def toString: String = s"Tensor@(samples, dimensions, rows(><)columns): $nsamples, $depth, $rows><$cols"

}

object Tensor extends TensorInstances {

  import Generator.rng

  def fromArray[A: ClassTag: Numeric](samples: Int, depth: Int, rows: Int, cols: Int, arr: Array[A]) =
    new Tensor(samples, depth, rows, cols)(Buffer.fromArray(arr))

  def ofSize[A: ClassTag: Numeric](depth: Int, rows: Int, cols: Int): Tensor[A] =
    new Tensor(0, depth, rows, cols)(Buffer.ofSize[A](depth * rows * cols))

  def empty[A: ClassTag: Numeric]: Tensor[A] = new Tensor(0, 0, 0, 0)(Buffer.empty[A])

  def zeros[A: ClassTag: Numeric](samples: Int, depth: Int, rows: Int, cols: Int)(implicit ev: Numeric[A]): Tensor[A] =
    new Tensor(samples, depth, rows, cols)(Buffer.fill(samples * depth * rows * cols)(ev.zero))

  def ones[A: ClassTag: Numeric](samples: Int, depth: Int, rows: Int, cols: Int)(implicit ev: Numeric[A]): Tensor[A] =
    new Tensor(samples, depth, rows, cols)(Buffer.fill(samples * depth * rows * cols)(ev.one))

  def uniform[A: ClassTag: Numeric](
      samples : Int,
      depth   : Int,
      rows    : Int,
      cols    : Int,
      scale   : (Double, Double) = (0.0, 1.0),
      rand    : Generator = rng)(implicit ev: ConvertableTo[A]): Tensor[A] = {
    val lower = scale._1
    val upper = scale._2
    val unidist = Uniform(lower, upper)
    val randstream = Stream.repeatEval(Task.delay(ev.fromDouble(unidist.apply(rand)))).take(samples * depth * rows * cols)
    new Tensor(samples, depth, rows, cols)(Buffer.unsafe(randstream.runLog.unsafeRun.toArray))
  }

  def uniform[A: ClassTag: Numeric](samples: Int, depth: Int, rows: Int, cols: Int, scale: Double): Tensor[A] =
    uniform[A](samples, depth, rows, cols, (-scale, scale))

  /**
   * Y. LeCun, L. Bottou, G. Orr, and K. Mueller.  Efficient BackProp.  In G. Orr and K. Mueller,
   * Neural Networks: tricks of the trade, pages 9-48.  Springer, 1998.
   */
  def lecun[A: ClassTag: Numeric](samples: Int, depth: Int, rows: Int, cols: Int, rand: Generator = rng): Tensor[A] =
    uniform[A](samples, depth, rows, cols, math.sqrt(3 / depth * rows * cols))

  /**
   * X. Glorot and Y. Bengio.  Understanding the difficulty of training deep feedforward neural networks.
   * In AISTATS, 2010.
   */
  def glorotUniform[A: ClassTag: Numeric](samples: Int, depth: Int, rows: Int, cols: Int, rand: Generator = rng): Tensor[A] =
    uniform[A](samples, depth, rows, cols, math.sqrt(2 / depth * rows * cols + samples * rows * cols))

}

abstract sealed class TensorInstances extends TensorInstances0

abstract sealed class TensorInstances0 extends TensorInstances1 {
  implicit def tensorCanShow[A](implicit ev: Show[A]): Show[Tensor[A]] =
    new Show[Tensor[A]] {
      def show(fa: Tensor[A]): String = fa.toString
    }
}

abstract sealed class TensorInstances1 {
  /**
  implicit val tensorsAreThese: TraverseFilter[Tensor] with MonadCombine[Tensor] with CoflatMap[Tensor] =
    new TraverseFilter[Tensor] with MonadCombine[Tensor] with CoflatMap[Tensor] {
      def empty[A]: Tensor[A] = Tensor.empty[A]

      def combineK[A](x: Tensor[A], y: Tensor[A]): Tensor[A] =
        if (x.depth == y.depth && x.rows == y.rows && x.cols == y.cols) {
          new Tensor(x.samples.value + y.samples.value, x.depth, x.rows, x.cols)(x.data ++ y.data)
        } else {
          val xydata = x.data ++ y.data
          new Tensor(xydata.length, 1, 1, 1)(xydata)
        }

      def pure[A](a: A): Tensor[A] =
        new Tensor(1, 1, 1, 1)(Buffer.fromArray(Array[A](a)))

      def coflatMap[A, B](fa: Tensor[A])(f: Tensor[A] => B): Tensor[B] = {
        @tailrec
        def inner(acc: VectorBuilder[B], as: Tensor[A]): Tensor[B] =
          as match {
            case t if t.hasTail => inner(acc += f(as), t.tailUnsafe)
            case t => new Tensor(fa.samples.value, fa.depth, fa.rows, fa.cols)(Buffer.unsafe(acc.result.toArray))
          }
        inner(new VectorBuilder[B], fa)
      }

      def flatMap[A, B](fa: Tensor[A])(f: A => Tensor[B]): Tensor[B] =
        foldLeft(fa, Tensor.empty[B]) { _ ++ f(_) }

      def foldLeft[A, B](fa: Tensor[A], b: B)(f: (B, A) => B): B =
        fa.data.elems.foldLeft(b)(f)

      def foldRight[A, B](fa: Tensor[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def inner(i: Int): Eval[B] = {
          if (i < fa.data.length) f(fa.data.apply(i), Eval.defer(inner(i + 1))) else lb
        }
        Eval.defer(inner(0))
      }

      def traverseFilter[G[_], A, B](fa: Tensor[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[Tensor[B]] = {
        val rf = foldRight[A, G[SVector[B]]](fa, Always(G.pure(SVector.empty))) { (a, lgvb) =>
          G.map2Eval(f(a), lgvb)((ob, v) => ob.fold(v)(_ +: v))
        }
        G.map[SVector[B], Tensor[B]](rf.value) { vec =>
          new Tensor(fa.samples.value, fa.depth, fa.rows, fa.cols)(Buffer.unsafe(vec.toArray))
        }
      }

      def tailRecM[A, B](a: A)(fn: A => Tensor[Either[A, B]]): Tensor[B] = {
        val buf = Buffer.empty[B]
        var state = List(fn(a).dataIterator)
        @tailrec
        def inner(): Unit = state match {
          case Nil => ()
          case h :: tail if h.isEmpty =>
            state = tail
            inner()
          case h :: tail =>
            h.next match {
              case Right(b) =>
                buf += b
                inner()
              case Left(a0) =>
                state = fn(a0).dataIterator :: h :: tail
                inner()
            }
        }
        inner()
        new Tensor(buf.length, 1, 1, 1)(buf)
      }

    }
    */
}
