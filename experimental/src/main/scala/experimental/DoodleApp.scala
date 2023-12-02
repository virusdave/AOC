package experimental

import cats.effect.unsafe.implicits.global
import cats.effect.{IO => CIO}
import cats.~>
import doodle.algebra
import fs2.{Pure, Chunk => FChunk, Stream => FStream}
import doodle.core._
import doodle.image._
import doodle.image.syntax._
import doodle.interact._
import doodle.interact.easing.Easing
import doodle.interact.effect.AnimationRenderer
import doodle.interact.syntax._
import doodle.java2d._
import doodle.java2d.effect.{Center, Size}
import doodle.java2d.examples.{BouncyCircles, Dash, PulsingCircle, Ripples}
import doodle.syntax._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import zio.clock.Clock
import zio.{App, Chunk, ExitCode, IO, RIO, Ref, Runtime, Schedule, Task, UIO, URIO, ZEnv, ZHub, ZIO, ZManaged}
import zio.console._
import zio.duration._
import zio.interop.catz._
import zio.stream.interop.fs2z._
import zio.stream.{Stream, ZStream}

object blah extends App {
//  LazyList().exists(_ => true)
//  LazyList().find(_ => true)
//  "123".substring()
  "123".view.slice(1, 2).foldLeft(true -> scala.collection.mutable.HashSet.empty[Char]) { case (distinct -> hs, c) =>
      if (distinct && !hs.contains(c)) {
        hs.addOne(c)
        true -> hs
      } else distinct -> scala.collection.mutable.HashSet.empty[Char]
  }._1

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = ???
}

//object DoodleApp extends App {
//  type RCIO[-R] = {
//    type CIO[A] = RIO[R, A]
//  }
//
//  // TODO(Dave): Move this elsewhere.
//  private object streams {
//    val catsEffectIoToZio: CIO ~> Task = new (CIO ~> Task) {
//      override def apply[A](fa: CIO[A]): Task[A] = fa.to[Task]
//    }
//    val zioToCatsEffectIo: Task ~> CIO = new (Task ~> CIO) {
//      implicit val ev: Runtime[Any]              = Runtime.default
//      override def apply[A](fa: Task[A]): CIO[A] = CIO.apply(ev.unsafeRun(fa))
//    }
//    val zioZenvToTask: RCIO[ZEnv]#CIO ~> Task = new (RCIO[ZEnv]#CIO ~> Task) {
//      override def apply[A](fa: RIO[ZEnv, A]): Task[A] = fa.provide(Runtime.default.environment)
//    }
//    def zioToTask[R](r: R): RCIO[R]#CIO ~> Task = new (RCIO[R]#CIO ~> Task) {
//      override def apply[A](fa: RIO[R, A]): Task[A] = fa.provide(r)
//    }
//    object syntax {
//      implicit class _FStreamSyntax[A](in: FStream[CIO, A]) {
//        def toStreamZio: FStream[Task, A]                        = in.translate(catsEffectIoToZio)
//        def toZStream(queueSize: Int = 16): Stream[Throwable, A] = in.toStreamZio.toZStream(queueSize)
//      }
//      implicit class _FStreamSyntaxPure[A](in: FStream[Pure, A]) {
//        def pureStream: Stream[Nothing, A] = ZStream.apply(in.toList: _*)
//      }
//      implicit class _FStreamToCIO[R, A](in: FStream[RCIO[R]#CIO, A]) {
//        def toCatsIOStream(implicit ev: ZEnv <:< R): FStream[CIO, A] =
//          ???
////          in.translate(zioZenvToTask).translate(zioToCatsEffectIo)
//        def toCatsIOStreamR[R2](r2: R2)(implicit ev: R2 with ZEnv <:< R): FStream[CIO, A] = ???
//      }
//    }
//  }
//  import streams.syntax._
//
//  override def run(args: List[String]): URIO[ZEnv, ExitCode] =
//    go.exitCode
//
//  val chessboard = {
//    val blackSquare = Image.rectangle(30, 30).fillColor(Color.black)
//    val redSquare   = Image.rectangle(30, 30).fillColor(Color.red)
//    val twoByTwo =
//      (redSquare.beside(blackSquare))
//        .above(blackSquare.beside(redSquare))
//
//    val fourByFour =
//      (twoByTwo.beside(twoByTwo))
//        .above(twoByTwo.beside(twoByTwo))
//
//    val chessboard =
//      (fourByFour.beside(fourByFour))
//        .above(fourByFour.beside(fourByFour))
//
//    chessboard
//  }
//
//  def go: RIO[ZEnv, Any] = {
//    def rotateSlowly(i: Image): RIO[ZEnv, Unit] = {
//      val frame = Frame.fitToPicture().title("Example")
//
//      val animation: FStream[RCIO[Clock]#CIO, Picture[Unit]] =
//        (Easing.linear.toStream(100).pureStream.forever <& ZStream.fromSchedule(Schedule.spaced(20.milliseconds))).map(
//          _.turns).map(i.rotate(_).compile[Algebra, Drawing]).toFs2Stream
//
////      val animation: FStream[cats.effect.IO, Picture[Unit]] = (FStream(1).repeat.debounce(FiniteDuration(
////        25,
////        TimeUnit.MILLISECONDS)) *> Easing.linear.toStream(100)).map(_.turns).map(t =>
////        i.rotate(t).compile[Algebra, Drawing]).repeat.lift[cats.effect.IO]
//
//      for {
////      canvas <- frame.canvas().to[Task]
////      _ <- canvas.redraw.compile.drain.to[Task]
//        _ <- AnimateStreamOps(animation.toCatsIOStream).animateToIO(frame).to[Task]
////        _ <- ZIO(i.draw())
////        _ <- ZIO.sleep(50.milliseconds)
//        //          _ <- rotateSlowly(i.rotate(90.degrees))
//      } yield ()
//    }
//    for {
//      _ <- putStrLn(s"rendering")
//      _ <- rotateSlowly(chessboard)
//      _ <- ZIO(Ripples.go())
//      _ <- ZIO.effectAsyncM[Any, Throwable, Unit](register =>
//        ZIO(BouncyCircles.animation.animate(BouncyCircles.frame, ZIO.fromEither(_) |> register)))
//      _ <- ZIO.effectAsyncM[Any, Throwable, Unit](register =>
//        ZIO(PulsingCircle.animation.animate(PulsingCircle.frame, ZIO.fromEither(_) |> register)))
//      _ <- ZIO.effectAsyncM[Any, Throwable, Unit](register =>
//        ZIO(Dash.animation.animate(Dash.frame, ZIO.fromEither(_) |> register)))
//      //        _ <- rotateSlowly(chessboard /*.rotate(90.degrees)*/ ).fork
//      _ <- putStrLn(s"done rendering, hit enter")
//      _ <- putStr("> ")
//      _ <- getStrLn
//      _ <- putStrLn(s"all done")
//    } yield ()
//  }
//}
