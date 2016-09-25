package nous.linalg
package internal

trait NArray[I, A] { self =>
  def dims: List[Idx[I]]

  def coords: Vector[A]

  def resetCoords(v: Vector[A]): NArray[I, A] =
    if (coords.dim == v.dim) {
      new NArray[I, A] {
        val dims = self.dims
        val coords = v
      }
    } else throw NArrayError("Coordinate replacement is of the incorrect size.")

  def mapDims(f: List[Idx[I]] => List[Idx[I]]): NArray[I, A] =
    new NArray[I, A] {
      val dims = f(self.dims)
      val coords = self.coords
    }

  def mapTypes[J](f: I => J): NArray[J, A] =
    new NArray[J, A] {
      val dims = self.dims map { idx => f(idx.idxType) }
      val coords = self.coords
    }

  def mapNames(f: String => String): NArray[I, A] =
    new NArray[I, A] {
      val dims = self.dims map { idx => f(idx.name) }
      val coords = self.coords
    }

  def renameWith(mapping: Map[String, String]): NArray[I, A] =
    new NArray[I, A] {
      val dims = mapNames { idxn =>
        mapping find { kv => kv._1 == idxn } match {
          case Some(kv) => kv._2
          case None => idxn
        }
      }
      val coords = self.coords
    }

  def namesR: List[String] = dims.map(_.name)

  def names: List[String] = namesR.sorted

  def size(idxname: String): Int = dims.filter(_.name == idxname).head.dim

  def sizesR: List[Int] = dims.map(_.dim)

  def typeOf(idxname: String): I = dims.filter(_.name == idxname).head.idxType

  def order: Int = dims.length

  def mapArray[B](f: Vector[A] => Vector[B]): NArray[I, B] =
      NArray.mkNArray(dims, f(coords))

}

object NArray {

  def mkNArray[I, A](dms: List[Idx[I]], vec: Vector[A]): NArray[I, A] = dms match {
    case Nil => throw NArrayError("Cannot create an narray with empty dimensions, use scalar instead.")
    case _ =>
      val ds = dms.map(_.dim)
      val n = ds.product
      val v = {
        if (vec.dim == n && ds.min > 0)
          vec
        else throw NArrayError(s"Cannot create narray with ${ds.toString} dimensions and ${vec.dim} coordinates.")
      }
      new NArray[I, A] {
        val dims = dms
        val coords = v
      }
  }

  def scalar[A](a: A): NArray[A, A] =
    new NArray[A, A] {
      val dims = List.empty[Idx[A]]
      val coords = Vector.fromList(List(a))
    }

}