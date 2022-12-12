package common

import scala.reflect.ClassTag
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
    def debugSameLine: A = {
      System.out.print("\r")
      tap(System.out.print)
    }
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

  implicit class _HomogeneousPairOps[A](private val in: (A, A)) {
    def min(implicit ev: Numeric[A]): A = ev.min(in._1, in._2)
    def max(implicit ev: Numeric[A]): A = ev.max(in._1, in._2)
  }

  implicit class _PairOps[A, B](private val in: (A, B)) {
  }

  implicit class _IntOps(private val in: Int) {
    def big: BigInt = BigInt(in)
  }
  implicit class _LongOps(private val in: Long) {
    def big: BigInt = BigInt(in)
  }

  implicit class _CharOps(private val in: Char) {
    /** "Slides" the current char by whatever "shift" it takes to move `from` to `to`
      *
      * Example:
      * 'C'.slide('A', 'D') == 'F', since this op moves 'A' to 'D', and 'C' is two after 'A', so it gets moved to
      * two after 'D', which is 'F'
      *
      * Be careful to not shift to an invalid character!
      *
      * NOTA BENE: This does NOT do a circular rotation or alphabet wrapping or anything!  It might not be super
      * useful as a result, so perhaps remove it if not.
      */
    def slide(from: Char, to: Char): Char = (in + to - from).toChar
  }

  implicit class _StringOps(private val in: String) {
    def big: BigInt = BigInt(in)

    def bigBinary: BigInt = BigInt(in, 2)

    def safeBig: Option[BigInt] = Try(in.big).toOption

    def safeBigBinary: Option[BigInt] = Try(in.bigBinary).toOption

    def mapChars(f: Char => Char): String = in.toSeq.map(f).mkString

    def splitAtLinebreaks: IndexedSeq[String] = in.linesIterator.toIndexedSeq
    def splitAtLinebreaksBy[A: ClassTag](f: String => A): IndexedSeq[A] = splitAtLinebreaks.map(f)
    def splitAtDoubleLinebreaks: IndexedSeq[String] = in.split("\n\n").toIndexedSeq
    def splitAtDoubleLinebreaksBy[A: ClassTag](f: String => A): IndexedSeq[A] =
      in.split("\n\n").toIndexedSeq.map(f)

    def splitToLinesAtDoubleLinebreaks: IndexedSeq[IndexedSeq[String]] =
      in.split("\n\n").toIndexedSeq.map(_.split("\n").toIndexedSeq)
    def splitToLinesAtDoubleLinebreaksBy[A: ClassTag](f: String => A): IndexedSeq[IndexedSeq[A]] =
      in.split("\n\n").toIndexedSeq.map(_.split("\n").map(f).toIndexedSeq)
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

    def getOrElse(x: Int, y: Int, orElse: B): B = get(x, y).getOrElse(orElse)

    def findInGridWithLocation(fn: (B, (Int, Int)) => Boolean): Option[(B, (Int, Int))] = {
      val (mx, my) = bounds
      (for {
        x <- 0 until mx
        y <- 0 until my
      } yield (x, y))
        .find { case (x, y) => get(x, y).exists(fn(_, x -> y)) }
        .map { case (x, y) => get(x, y).get -> (x -> y) }
    }
    def findInGrid(fn: B => Boolean): Option[(B, (Int, Int))] = findInGridWithLocation { case (b, _) => fn(b) }

    def locationsInGridWithLocation(fn: (B, (Int, Int)) => Boolean): Seq[(B, (Int, Int))] = {
      val (mx, my) = bounds
      for {
        x <- 0 until mx
        y <- 0 until my
        v <- get(x, y).toSeq
        _ <- Seq(1).filter(_ => fn(v, (x, y)))
      } yield v -> (x -> y)
    }
    def locationsInGrid(fn: B => Boolean): Seq[(B, (Int, Int))] = locationsInGridWithLocation { case (b, _) => fn(b) }

    def clip(x: Int, y: Int): Option[(Int, Int)] = {
      val (mx, my) = bounds
      (x, y).some.filter { case (x, y) => x >= 0 && x < mx && y >= 0 && y < my }
    }

    def bounds: (Int, Int) = (in(0).length, in.length)

    /** Map via `f` at each location in the grid, passing the contents and the coordinates to `f` */
    def mapGridWithLocation[C](
        f: (B, (Int, Int)) => C
    )(
        implicit ev: A <:< IndexedSeq[B]
    ): Grid[C] = in.zipWithIndex.map { case (inner, y) =>
      inner.zipWithIndex.map { case (b, x) => f(b, (x, y)) }
    }

    def mapGrid[C](f: B => C): Grid[C] = in.map(_.map(f))

    def show[BB](f: B => BB): String =
      in.map(_.map(f).mkString).mkString("\n", "\n", "")

    def rotate90cw: Grid[B] = in.transpose.reflectHorizontal

    def rotate90ccw: Grid[B] = in.transpose.reflectVertical

    def reflectHorizontal: Grid[B] = in.map(_.reverse)

    def reflectVertical: IndexedSeq[A] = in.reverse
  }

  type Grid[A] = IndexedSeq[IndexedSeq[A]]
  implicit class GridCompanionOps(in: IndexedSeq.type) {
    def fill2d[A](x: Int, y: Int)(a: => A): Grid[A] = in.fill(y, x)(a)
  }
  val Grid: IndexedSeq.type = IndexedSeq
}
