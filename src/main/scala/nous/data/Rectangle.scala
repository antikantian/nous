package nous.data

case class Rectangle(left: Int, top: Int, right: Int, bottom: Int) {
  def contains(x: Int, y: Int): Boolean =
    if (x < 1 || x > right || y < top || y > bottom) false else true
}