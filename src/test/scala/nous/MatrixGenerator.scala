package nous

import nous.data.Matrix
import org.scalacheck.Properties
import org.scalacheck.{ Arbitrary, Gen, Properties }
import org.scalacheck.Prop.forAll
import spire.math.Numeric

object MatrixGenerator {
  def matrixDoubleZerosGen: Gen[Matrix[Double]] =
    for {
      row <- Gen.choose(0, 9999)
      col <- Gen.choose(0, 9999)
    } yield Matrix.zeros[Double](row, col)

  implicit val arbMatrix: Arbitrary[Matrix[Double]] = Arbitrary(matrixDoubleZerosGen)

}