package codekata2022
package days

import common.InRegexParserSyntax

object Day07 extends ParserPuzzle with InRegexParserSyntax {
  override type PuzzleOut = Any
  override def dayNum: Int = 7

  private type Path = String
  private type Size = Int

  private sealed trait Listing
  private case class Dir(name: String) extends Listing
  private case class File(size: Size, name: String) extends Listing

  private sealed trait Command
  private case object CdRoot extends Command
  private case object CdUp extends Command
  private case class Cd(where: String) extends Command
  private case class Ls(contents: Seq[Listing]) extends Command

  private val num: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  private val filename: Parser[String] = "[a-zA-Z0-9.]+".r
  private val cd: Parser[Command] =
    "$" ~> "cd" ~> ("/" ^^^ CdRoot | ".." ^^^ CdUp | filename ^^ Cd)
  private val ls: Parser[Ls] = {
    val dir = "dir" ~> filename ^^ Dir
    val file = num ~ filename ^^ (_.toTuple) ^^ (File.apply _).tupled
    "$" ~> "ls" ~> rep(dir | file) ^^ Ls
  }
  private val terminalOutputParser = rep1(cd | ls)
  private val parsedTerminal = parseAll(terminalOutputParser, inputs).get

  private case class TerminalState(
      cwd: Seq[String] = Seq.empty,
      files: Map[Path, Size] = Map.empty,
      dirs: Set[Path] = Set.empty,
  )
  private val filesystemContents = parsedTerminal.foldLeft(TerminalState()) {
    case (state@TerminalState(cwd, files, dirs)) -> command =>
      command match {
        case CdRoot => state.copy(cwd = Seq.empty)
        case CdUp => state.copy(cwd = cwd.drop(1))
        case Cd(where) => state.copy(cwd = where +: cwd)
        case Ls(listing) =>
          val path = cwd.reverse
          state.copy(
            files = files ++ listing.flatMap {
              case _: Dir => Seq.empty
              case File(size, name) => Seq((path :+ name).mkString("/", "/", "") -> size)
            }.toMap,
            dirs = dirs ++ listing.flatMap {
              case Dir(name) => Seq((path :+ name).mkString("/", "/", ""))
              case _: File => Seq.empty
            })
      }
  }

  private val dirsWithCumulativeContentSize: Seq[(Path, Size)] = {
    val files = filesystemContents.files.toSeq
    (filesystemContents.dirs + "/").toSeq.map { dir =>
      dir -> files.filter(_._1.startsWith(dir)).map(_._2).sum
    }
  }

  override def part1: Option[Part] = PuzzlePart({
    dirsWithCumulativeContentSize.filter(_._2 <= 100000).map(_._2).sum
  }.zio).some

  override def part2: Option[Part] = PuzzlePart({
    val diskSize = 70000000
    val updateNeeded = 30000000
    val filesUsed = filesystemContents.files.toSeq.map(_._2).sum
    val freeSpace = diskSize - filesUsed
    val freeNeeded = updateNeeded - freeSpace
    dirsWithCumulativeContentSize.sortBy(_._2).dropWhile(_._2 < freeNeeded).head._2
  }.zio).some

  private def inputs = in2

  private lazy val in2 =
    """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

  private lazy val in3 =
    """"""

  override def in: String =
    """""".stripMargin
}
