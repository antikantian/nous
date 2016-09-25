package nous.instances

import nous.linalg.Vector
import nous.linalg.internal.ElementTag

object double {

  trait DoubleInstances {
    implicit final val doubleHasElementTag = new ElementTag[Double] {
      def isValid: Boolean = true

      def gpuCompatible: Boolean = false

      def bitSize: Int = java.lang.Double.SIZE

      def makeVector(arr: Array[Double]) = new Vector[Double](arr)
    }
  }

}