package nous.free

import cats.free.Free
import freasymonad.cats.free
import nous.data._

@free trait Layer {

  type LayerF[A] = Free[LayerADT, A]
  sealed trait LayerADT[A]

  def forward[A](x: Sample[A, A], w: Weights[A]): LayerF[Sample[A, A]]
  def backward[A](g: Array[A], xa: Array[A]): LayerF[Array[A]]

}