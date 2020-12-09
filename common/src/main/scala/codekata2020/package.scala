import codekata2020.common._
import zio.{UIO, ZIO}
import zio.console.Console

package object codekata2020 {

  implicit class _AnyOps[A](private val in: A) extends AnyVal {
    /** Lift a value into an [[Option]], but with better type inference than `Some(in)` */
    def some: Option[A] = Option(in)

    /** Pipe operator.  Pipe a value `v` into function `f` with `v |> f`. */
    def |>[B](fn: A => B): B = pipe(fn)
    def pipe[B](fn: A => B): B = fn(in)

    def dup: (A, A) = (in, in)
    def diag[B](fn: (A, A) => B): B = fn(in, in)

    def zio: UIO[A] = UIO.succeed(in)
  }

  implicit class _ZIOOps[R, E, A](val in: ZIO[R, E, A]) extends AnyVal {
    /** Apply an aspect to an effectful program */
    def @@[R1 <: R, E1 >: E](aspect: ZAspect[R1, E1]): ZIO[R1, E1, A] = aspect(in)

    def shush(silent: Boolean): ZIO[R with Console, E, A] = if (silent) (in @@ NoConsoleOutput) else in
  }
}
