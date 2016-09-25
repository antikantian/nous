package nous
package syntax

import nous.linalg.Matrix
import nous.syntax.ops.MatrixOps

trait MatrixSyntax {
  implicit def nousSyntaxMatrix[A](ma: Matrix[A]): MatrixOps[A] =
    new MatrixOps[A](ma)
}