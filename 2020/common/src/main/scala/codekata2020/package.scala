import common.Syntax
import scala.collection
import scala.util.Try
import zio.{UIO, ZIO}
import zio.console.Console

package object codekata2020 extends Syntax {

  implicit class _GridOps[A](private val in: IndexedSeq[A]) extends AnyVal {
    /** Safely get the contents of (x,y), or None if that's out of bounds. */
    def get[B](x: Int, y: Int)(implicit ev: A <:< IndexedSeq[B]): Option[B] =
      if (y >= 0 && y < in.length && x >= 0 && x < in(y).length) in(y)(x).some else None

    // TODO(Dave): This really sucks for type inference, for passing anonymous functions in.
    /** Map via `f` at each location in the grid, passing the contents and the coordinates to `f` */
    def mapGridWithLocation[B, C](
      f: (B, (Int, Int)) => C)(
      implicit ev: A <:< IndexedSeq[B])
    : IndexedSeq[IndexedSeq[C]] =
      in.zipWithIndex.map { case (inner, y) =>
        inner.zipWithIndex.map { case (b, x) => f(b, (x, y)) }
      }

    def rotate90cw[B](implicit ev: A <:< IndexedSeq[B]): IndexedSeq[IndexedSeq[B]] = in.transpose.reflectHorizontal
    def rotate90ccw[B](implicit ev: A <:< IndexedSeq[B]): IndexedSeq[IndexedSeq[B]] = in.transpose.reflectVertical
    def reflectHorizontal[B](implicit ev: A <:< IndexedSeq[B]): IndexedSeq[IndexedSeq[B]] = in.map(_.reverse)
    def reflectVertical[B](implicit ev: A <:< IndexedSeq[B]): IndexedSeq[A] = in.reverse
  }
}
