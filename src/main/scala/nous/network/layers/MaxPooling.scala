package nous.network.layers

import nous.kernels.Blas

/**
class MaxPooling[A](cdef: MaxPoolDefinition[A], input: Shape4D, w: Vector[A])(
    implicit mat: Blas[A]) extends HiddenLayer[A] { self =>

  val definition = cdef
  val weights = w

  val height = definition.poolH
  val width = definition.poolW
  val stride = definition.stride
  val padding = definition.padding

}

 */