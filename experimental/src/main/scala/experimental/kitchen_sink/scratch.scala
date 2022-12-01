package experimental
package kitchen_sink

import zio._
import zio.logging._
import zio.magic._

object scratch extends App {
  type ENV = Logging

  private val env: URLayer[ZEnv, ENV] = {
    val logging = Logging.console() >>> Logging.withRootLoggerName("scratch")

    ZLayer.wireSome[ZEnv, ENV](
      logging,
    )
  }

  private val program: RIO[ENV with ZEnv, Unit] = for {
    _ <- ZIO.unit
    _ <- log.warn(s"We're alive!")
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program
      .injectCustom(env)
      .exitCode
}