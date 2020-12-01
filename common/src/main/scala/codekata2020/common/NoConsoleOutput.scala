package codekata2020.common

import java.io.IOException
import zio.{IO, UIO, ZIO}
import zio.console.Console

object NoConsoleOutput extends ZAspect[Console, Nothing] {
  override def apply[R1 <: Console, E1 >: Nothing, A](zio: ZIO[R1, E1, A]): ZIO[R1, E1, A] =
    zio.updateService[Console.Service] { console =>
      new Console.Service {
        override def putStr(line: String): UIO[Unit] = IO.unit
        override def putStrErr(line: String): UIO[Unit] = IO.unit
        override def putStrLn(line: String): UIO[Unit] = IO.unit
        override def putStrLnErr(line: String): UIO[Unit] = IO.unit

        override def getStrLn: IO[IOException, String] = console.getStrLn
      }
    }
}