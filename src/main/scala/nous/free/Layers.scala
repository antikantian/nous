package nous.free

import cats.free.Free
import freasymonad.free
import nous.layers._
import spire.math.Numeric

@free trait Layers {

  type LayerDef[A] = Free[LayerOp, A]

  sealed trait LayerOp[A]

  def fullyconnected[O: Numeric](outputs: Int, hasBias: Boolean): LayerDef[FullyConnected[O]]

  def convolution[O: Numeric](filters: Int, rows: Int, cols: Int, strideH: Int, strideW: Int): LayerDef[Convolutional[O]]

  def dropout[O: Numeric](rate: Double): LayerDef[Dropout[O]]

  def multiply[O: Numeric](constant: Double): LayerDef[Multiply[O]]

  def batchnorm[O: Numeric](mode: Mode): LayerDef[BatchNormalization[O]]

  def affine[O: Numeric](mode: Mode): LayerDef[Affine[O]]

  def maxpool[O: Numeric](rows: Int, cols: Int, strideH: Int, strideW: Int): LayerDef[MaxPooling[O]]

  def avgpool[O: Numeric](rows: Int, cols: Int, strideH: Int, strideW: Int): LayerDef[AveragePooling[O]]

  def relu[O: Numeric]: LayerDef[ReLU[O]]

  def prelu[O: Numeric]: LayerDef[PReLU[O]]

  def sigmoid[O: Numeric]: LayerDef[Sigmoid[O]]

  def htan[O: Numeric]: LayerDef[HyperTan[O]]

  def softmax[O: Numeric]: LayerDef[Softmax[O]]

  def lambda[O: Numeric](f: Matrix[O] => Matrix[O]): LayerDef[Lambda[O]]

}