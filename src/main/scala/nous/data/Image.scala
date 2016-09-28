package nous.data

import java.io.ByteArrayInputStream

import com.sksamuel.scrimage.{ Image => SImg, Pixel }
import scalaj.http._
import spire.math.Numeric

class Image(underlying: SImg) {

  def red[A](implicit ev: Numeric[A]) = underlying.pixels.map(pix => Pixel(pix.red, 0, 0, pix.alpha))

  def green[A](implicit ev: Numeric[A]) = underlying.pixels.map(pix => Pixel(0, pix.green, 0, pix.alpha))

  def blue[A](implicit ev: Numeric[A]) = underlying.pixels.map(pix => Pixel(0, 0, pix.blue, pix.alpha))

}

object Image {

  def fromUrl[A](url: String): Image = {
    val img = Http(url).asBytes.body
    val in = new ByteArrayInputStream(img)
    new Image(SImg.fromStream(in))
  }

}