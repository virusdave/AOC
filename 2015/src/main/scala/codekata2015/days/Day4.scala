package codekata2015
package days

import java.security.MessageDigest

object Day4 extends Puzzle {
  override type A = Option[Int]

  override def dayNum: Int = 4

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }


  override def part1: Option[Part] = new Part {
    override def solution = LazyList.from(1).find(n => md5(s"${inputs}${n}").startsWith("00000")).zio
  }.some

  override def part2: Option[Part] = new Part {
    override def solution = LazyList.from(1).find(n => md5(s"${inputs}${n}").startsWith("000000")).zio
  }.some

  def inputs = in2

  val in2 = "abcdef"
  val in3 = "pqrstuv"

  override def in: String = """"""
}