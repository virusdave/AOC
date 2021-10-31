package codekata2016
package days

import java.security.MessageDigest
import zio.blocking._
import zio.console._
import zio.stream.UStream
import zio.{Promise, RIO, RefM, ZEnv, ZIO}

object Day05 extends ParserPuzzle {
  override type PuzzleOut = Any
  override def dayNum: Int = 5

  private val primedHash: String => String => String = (prefix: String) => {
    val md5 = MessageDigest.getInstance("MD5").tap(_.update(prefix.getBytes))
    (str: String) => md5.clone().asInstanceOf[MessageDigest].digest(str.getBytes).map("%02x".format(_)).mkString
  }
  val hash = primedHash(inputs)

  override def part1: Option[Part] = new Part {
    override def solution: RIO[Blocking, Any] =
      UStream.iterate(1)(_ + 1)
        .mapMPar(50)(n => blocking(ZIO.succeed(hash(n.toString))))
        .filter(_.startsWith("00000"))
        .map(_.slice(5, 6))
        .take(8)
        .runCollect
        .map(_.mkString(""))

  }.some

  override def part2: Option[Part] = new Part {
    override def solution: RIO[ZEnv, Any] = {
      def show(m: Map[Int, String]) = (0 to 7).map(m.get).map(_.getOrElse("_")).mkString
      for {
        digits <- RefM.make(Map.empty[Int, String])
        stop   <- Promise.make[Nothing, Unit]
        _ <- UStream.iterate(1)(_ + 1)
          .chunkN(10240)
          .mapMPar(50)(n => blocking(
            // putStrLn(s"Trying $n").when(n % 2500000 == 0) *>
              ZIO.succeed(hash(n.toString))))
          .filter(_.startsWith("00000"))
          .map(x => (x.slice(5, 7).splitAt(1)))
          .takeUntilM(_ => stop.isDone)
          .foreach { case (l, r) =>
            val pos = l.toIntOption.getOrElse(10)
            digits.update { d =>
              if (pos >= 8 || d.contains(pos)) ZIO.succeed(d)
              else {
                val newD       = d + (pos -> r)
                val throwLatch = stop.succeed(()).when((0 to 7).forall(newD.contains))
                putStrLn(show(newD)).as(newD) <* throwLatch
              }
            }
          }
        out <- digits.map(show).get
      } yield out
    }
  }.some

  def inputs = in2

  lazy val in2 = "abc"
  lazy val in3 = ""

  override def in: String =
    """""".stripMargin
}
