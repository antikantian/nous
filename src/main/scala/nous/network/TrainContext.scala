package nous.network

case class TrainContext(epoch: Int, interval: Int, error: Double, derivative: Double)