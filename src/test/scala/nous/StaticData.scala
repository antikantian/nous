package nous

import nous.data.{ Batch, Sample }

object StaticData {

  val input5x5x3 =
    Vector[Double](
      2, 1, 2, 0, 2,
      2, 1, 1, 2, 0,
      2, 0, 1, 2, 1,
      0, 0, 0, 2, 0,
      0, 2, 1, 1, 2,
      2, 0, 2, 0, 0,
      1, 2, 0, 0, 0,
      1, 1, 2, 2, 2,
      1, 2, 0, 2, 2,
      0, 2, 1, 1, 0,
      0, 2, 2, 2, 1,
      0, 0, 0, 2, 1,
      2, 0, 1, 0, 0,
      0, 1, 2, 2, 0,
      2, 1, 2, 1, 1)

  val w03x3x3 =
    Vector[Double](-1, 1, -1, 1, 1, 1, 1, 1, 0, 0, 1, -1, -1, -1, 1, -1, -1, 1, 1, -1, 0, 0, 0, -1, 0, 0, -1)

  val w13x3x3 =
    Vector[Double](-1, 0, -1, -1, 0, 0, 0, -1, -1, 1, 0, -1, -1, 0, 1, -1, -1, 1, 0, 1, 1, -1, 0, 1, 0, -1, 1)

  val filter3x3x3 = w03x3x3 ++ w13x3x3

  val bias1x1x2 = Vector[Double](1, 0)

  val sample3x5x5 = new Sample(3, 5, 5, input5x5x3, 999d)

  val batch1x3x5x5 = new Batch(Vector(sample3x5x5))

}