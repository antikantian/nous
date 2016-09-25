package nous

import cats._
import nous.optimizers._

trait Model[Net[_], L] extends Monad[Net] {

  def add(layer: L): Net[Unit]
  def build(opt: Optimizer, loss: String): Net[Unit]
  def train[D, T](data: D, targets: T): Net[Unit]
  def train1[D, T](data: D, targets: T): Net[Unit]
  def test[D, T](valdata: D, valtargets: T): Net[Unit]
  def predict[D, O](x: D): O

}

object Model {
  def apply[Net[_], L](implicit Net: Model[Net, L]): Model[Net, L] = Net
}