package nous

import org.scalacheck.Properties
import org.scalacheck.{ Arbitrary, Gen, Properties }
import org.scalacheck.Prop.forAll
import spire.math.Numeric

object DataGenerator {

  def inputDoubleGen(n: Int): Gen[Vector[Double]] = Gen.containerOfN[Vector, Double](n, Gen.choose(0, 999))

}