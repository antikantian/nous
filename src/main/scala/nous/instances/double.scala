package nous.instances

/**

import nous.data.Vector

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

*/