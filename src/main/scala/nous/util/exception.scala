package nous.util

object exception {

  sealed abstract class NousException(msg: String) extends Exception(msg)

  sealed abstract class BundleException(msg: String) extends NousException(msg)
  case class BundleSamplesMismatch(msg: String) extends BundleException(msg)

  sealed abstract class SampleException(msg: String) extends NousException(msg)
  case class SampleMismatch(msg: String) extends SampleException(msg)

}