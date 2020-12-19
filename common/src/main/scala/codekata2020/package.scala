import codekata2020.common._
import scala.util.Try
import zio.{UIO, ZIO}
import zio.console.Console

package object codekata2020 {

  implicit class _AnyOps[A](private val in: A) extends AnyVal {
    /** Lift a value into an [[Option]], but with better type inference than `Some(in)` */
    def some: Option[A] = Option(in)

    /** Pipe operator.  Pipe a value `v` into function `f` with `v |> f`. */
    def |>[B](f: A => B): B = pipe(f)
    def pipe[B](f: A => B): B = f(in)

    /** Apply side-effecting function `f` on the value, returning the value.  Useful for debugging. */
    def tap(f: A => Unit): A = { f(in); in }

    /** Turn a value into two copies of the value */
    def dup: (A, A) = (in, in)
    /** Apply a binary function to this value, as both arguments */
    def diag[B](fn: (A, A) => B): B = fn(in, in)

    def zio: UIO[A] = UIO.succeed(in)
    def debug: A = tap(System.out.println)
  }

  implicit class _BooleanOps(private val in: Boolean) extends AnyVal {
    def toInt: Int = if (in) 1 else 0
  }

  implicit class _IntOps(private val in: Int) extends AnyVal {
    def big: BigInt = BigInt(in)
  }

  implicit class _StringOps(private val in: String) extends AnyVal {
    def big: BigInt = BigInt(in)
    def bigBinary: BigInt = BigInt(in, 2)
    def safeBig: Option[BigInt] = Try(in.big).toOption
    def safeBigBinary: Option[BigInt] = Try(in.bigBinary).toOption
  }

  implicit class _ZIOOps[R, E, A](private val in: ZIO[R, E, A]) extends AnyVal {
    /** Apply an aspect to an effectful program */
    def @@[R1 <: R, E1 >: E](aspect: ZAspect[R1, E1]): ZIO[R1, E1, A] = aspect(in)

    def shush(silent: Boolean): ZIO[R with Console, E, A] = if (silent) (in @@ NoConsoleOutput) else in
  }

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
  }
}
