package nous.free

object interpreters {

  /**

  type NetworkDefinition[F[_], A, B] = State[Vector[LayerDefinition[A]], B]
  val defaultLayerBuilder: LayerDefOp ~> NetworkDefinition =
    new (LayerDefOp ~> NetworkDefinition) {
      def apply[F[_], A, B, C](fa: LayerDefOp[C]): NetworkDefinition[F, A, B] =
        fa match {
          case x: ConvDefinition[A] =>
            State[Vector[LayerDefinition[A]], B] { layers =>
              (layers :+ ConvDefinition(x.filters, x.height, x.width, x.stride, x.padding, x.bias, x.initialization, x.activation, x.lambda), Trivial)
            }
        }
    }

   */
}