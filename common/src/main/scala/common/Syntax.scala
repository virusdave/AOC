package common

import scala.util.Try
import zio.{UIO, ZIO}
import zio.console.Console

trait Syntax {

  implicit class _AnyOps[A](private val in: A) {
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

  implicit class _OptionOps[A](private val in: Option[A]) {
    def fold2[B](ifNone: => B, ifSome: A => B): B = in.fold(ifNone)(ifSome)
  }

  implicit class _BooleanOps(private val in: Boolean) {
    def toInt: Int = if (in) 1 else 0
  }

  implicit class _IntOps(private val in: Int) {
    def big: BigInt = BigInt(in)
  }

  implicit class _StringOps(private val in: String) {
    def big: BigInt = BigInt(in)
    def bigBinary: BigInt = BigInt(in, 2)
    def safeBig: Option[BigInt] = Try(in.big).toOption
    def safeBigBinary: Option[BigInt] = Try(in.bigBinary).toOption
  }

  implicit class _ZIOOps[R, E, A](private val in: ZIO[R, E, A]) {
    /** Apply an aspect to an effectful program */
    def @@[R1 <: R, E1 >: E](aspect: ZAspect[R1, E1]): ZIO[R1, E1, A] = aspect(in)

    def shush(silent: Boolean): ZIO[R with Console, E, A] = if (silent) (in @@ NoConsoleOutput) else in
  }

}