package nous.network
package loss

import nous.network.layers._

trait LossF[A] {
  def f(o: NetworkOutput[A]): Error[A]
  def df(o: NetworkOutput[A]): Vector[A]
}