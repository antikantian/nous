package nous.data

import scala.reflect.ClassTag

import spire.algebra._
import spire.implicits._
import spire.math._

class Channel[A: ClassTag](h: Int, w: Int, elems: Array[A]) { self =>
  val data = elems
  val height = h
  val width = w
  val size = height * width

  // Assuming row-major
  def at(x: Int, y: Int): A = data(x + y * w)

  def subchannel(r: Rectangle) = window((r.left, r.top), (r.right, r.bottom))

  def window(top_left: (Int, Int), bottom_right: (Int, Int)) = {
    val (tx, ty) = top_left
    val (bx, by) = bottom_right

    for {
      x <- tx until bx
      y <- ty until by
    } yield at(x, y)
  }

  def update(e: Array[A]): Channel[A] = {
    require(e.length == height * width, "Updated data length is inconsistent with previous dimensions, use updated(h, w, e) instead")
    update(h, w, e)
  }

  def update(h: Int, w: Int, e: Array[A]): Channel[A] = new Channel(h, w, e)

}

object Channel {
  def apply[A: ClassTag](h: Int, w: Int, data: Array[A]): Channel[A] = new Channel(h, w, data)
}