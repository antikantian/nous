package nous.data

class Rectangle(val left: Int, val top: Int, val right: Int, val bottom: Int) {

  def contains(x: Int, y: Int): Boolean =
    if (x < 1 || x > right || y < top || y > bottom) false else true

  def width = right - left + 1

  def height = bottom - top + 1

  def isEmpty: Boolean = top > bottom || left > right

  def intersect(rhs: Rectangle): Rectangle = {
    val l = math.max(left, rhs.left)
    val t = math.max(top, rhs.top)
    val r = math.min(right, rhs.right)
    val b = math.min(bottom, rhs.bottom)
    new Rectangle(l, t, r, b)
  }

  override def toString = s"Rectangle($left, $top, $right, $bottom)"

}

object Rectangle {

  def apply(left: Int, top: Int, right: Int, bottom: Int): Rectangle =
    new Rectangle(left, top, right, bottom)

  def apply(w: Int, h: Int): Rectangle =
    new Rectangle(0, 0, w - 1, h - 1)

  def centered(x: Int, y: Int, w: Int, h: Int) = {
    val left = (x - w) / 2
    val top = (y - h) / 2
    val right = left + w - 1
    val bottom = top + h - 1
    new Rectangle(left, top, right, bottom)
  }

}