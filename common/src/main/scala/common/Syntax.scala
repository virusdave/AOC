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
    def tap(f: A => Unit): A = {
      f(in);
      in
    }

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

  implicit class _SeqOps[A](private val in: Seq[A]) {
    private def combs[B](n: Int, l: List[B]): Iterator[List[B]] = n match {
      case _ if n < 0 || l.lengthCompare(n) < 0 => Iterator.empty
      case 0 => Iterator(List.empty)
      case n => l.tails.flatMap({
        case Nil => Nil
        case x :: xs => combs(n - 1, xs).map(x :: _)
      })
    }
    def combinationsWithRepetition(n: Int): Iterator[Seq[A]] = combs(n, in.toList).map(_.toSeq)
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

  implicit class _GridOps[A, B](private val in: IndexedSeq[A])(implicit ev: A <:< IndexedSeq[B]) {
    /** Safely get the contents of (x,y), or None if that's out of bounds. */
    def get(x: Int, y: Int): Option[B] =
      if (y >= 0 && y < in.length && x >= 0 && x < in(y).length) in(y)(x).some else None

    /** Map via `f` at each location in the grid, passing the contents and the coordinates to `f` */
    def mapGridWithLocation[C](
        f: (B, (Int, Int)) => C)(
        implicit ev: A <:< IndexedSeq[B])
    : IndexedSeq[IndexedSeq[C]] = in.zipWithIndex.map { case (inner, y) =>
      inner.zipWithIndex.map { case (b, x) => f(b, (x, y)) }
    }

    def rotate90cw: IndexedSeq[IndexedSeq[B]] = in.transpose.reflectHorizontal

    def rotate90ccw: IndexedSeq[IndexedSeq[B]] = in.transpose.reflectVertical

    def reflectHorizontal: IndexedSeq[IndexedSeq[B]] = in.map(_.reverse)

    def reflectVertical: IndexedSeq[A] = in.reverse
  }
}