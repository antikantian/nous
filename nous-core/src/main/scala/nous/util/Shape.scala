package nous.util

case class Shape(n: Int, k: Int, r: Int, c: Int) {
  def isRowVector = n == 1 && k == 1 && r > 1 && c == 1

  def isColumnVector = n == 1 && k == 1 && r == 1 && c > 1

  def isVector = isRowVector || isColumnVector

  def length = n * k * r * c

}