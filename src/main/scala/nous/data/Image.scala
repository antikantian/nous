package nous.data

import java.io.ByteArrayInputStream

import cats._
import com.sksamuel.scrimage.{ Image => SImg, Pixel }
import fs2._
import scalaj.http._
import spire.math.Numeric

class Image(underlying: SImg) {

  val height = underlying.height
  val width = underlying.width
  val depth = 3

  def alpha[A](implicit ev: Numeric[A]) =
    Stream
      .emits(underlying.pixels)
      .map(pix => pix.alpha)

  def red[A](implicit ev: Numeric[A]) =
    Stream
      .emits(underlying.pixels)
      .map(pix => ev.fromInt(pix.red))

  def green[A](implicit ev: Numeric[A]) =
    Stream
    .emits(underlying.pixels)
    .map(pix => ev.fromInt(pix.green))

  def blue[A](implicit ev: Numeric[A]) =
    Stream
      .emits(underlying.pixels)
      .map(pix => ev.fromInt(pix.blue))

  override def toString = underlying.toString

}

object Image {

  def fromUrl[A](url: String): Image = {
    val img = Http(url).asBytes.body
    val in = new ByteArrayInputStream(img)
    new Image(SImg.fromStream(in))
  }

}