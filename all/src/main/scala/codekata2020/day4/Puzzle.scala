package codekata2020.day4

import codekata2020._
import scala.util.Try

object Puzzle {
  private val inputs = in2
    .replaceAll("\n\n", "ZZZZZZ")  // How disgusting.
    .replaceAll("\n", " ")
    .replaceAll("ZZZZZZ", "\n")
    .linesIterator
    .toIndexedSeq

  private val requiredKeys = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"/*, "cid"*/)

  object Part1 {
    def solution = {
      inputs.map(_.split(" ").map(_.split(":").head))  // Lines of "keys before the colon"
        .count(keys => keys.count(requiredKeys.apply) == requiredKeys.size)
        .zio
    }
  }

  object Part2 {
    def solution = {
      val validators: Map[String, String => Boolean] = Map(
        "byr" -> { x => x.length == 4 && Try { x.toInt >= 1920 && x.toInt <= 2002 }.getOrElse(false) },
        "iyr" -> { x => x.length == 4 && Try { x.toInt >= 2010 && x.toInt <= 2020 }.getOrElse(false) },
        "eyr" -> { x => x.length == 4 && Try { x.toInt >= 2020 && x.toInt <= 2030 }.getOrElse(false) },
        "hgt" -> { x =>
          val parse = "(\\d*)(cm|in)".r
          x match {
            case parse(num, unit) =>
              if (unit == "cm") num.toInt >= 150 && num.toInt <= 193
              else if (unit == "in") num.toInt >= 59 && num.toInt <= 76
              else false
            case _ => false
          }
        },
        "hcl" -> { "#[0-9a-f]{6}".r.matches },
        "ecl" -> { "amb|blu|brn|gry|grn|hzl|oth".r.matches },
        "pid" -> { "\\d{9}".r.matches }
      )
      inputs.map(_.split(" ").map { w =>
        val kv = w.split(":")
        kv(0) -> kv(1)
      }.toMap).count { kvs =>
        kvs.keys.count(requiredKeys.apply) == requiredKeys.size &&
        kvs.count { kv => validators.getOrElse(kv._1, (_: String) => false)(kv._2) } == requiredKeys.size
      }.zio
    }
  }

  private lazy val in2 =
    """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
      |byr:1937 iyr:2017 cid:147 hgt:183cm
      |
      |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
      |hcl:#cfa07d byr:1929
      |
      |hcl:#ae17e1 iyr:2013
      |eyr:2024
      |ecl:brn pid:760753108 byr:1931
      |hgt:179cm
      |
      |hcl:#cfa07d eyr:2025 pid:166559648
      |iyr:2011 ecl:brn hgt:59in""".stripMargin

  private lazy val in: String =
    // Puzzle input goes here.
    """""".stripMargin
}
