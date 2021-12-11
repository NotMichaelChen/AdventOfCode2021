package mzc.day10

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {
  def syntaxCheck(line: List[Char]): Either[Char, String] = {
    val bracketMap = Map(
      ')' -> '(',
      ']' -> '[',
      '}' -> '{',
      '>' -> '<'
    )

    val bracketMapInverse = bracketMap.map(_.swap)

    @tailrec
    def loop(line: List[Char], stack: List[Char]): Either[Char, String] = {
      line.headOption match {
        case Some(nextBracket) if bracketMap.contains(nextBracket) && stack.headOption.contains(bracketMap(nextBracket)) =>
          loop(line.tail, stack.tail)
        case Some(nextBracket) if bracketMap.contains(nextBracket) && stack.headOption.exists(_ != bracketMap(nextBracket)) =>
          Left(nextBracket)
        case Some(nextBracket) => loop(line.tail, nextBracket +: stack)
        case None => Right(stack.map(bracketMapInverse.apply).mkString)
      }
    }

    loop(line, List.empty)
  }

  def scoreLine(line: String): Long = {
    val scoreMap = Map(
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4
    )
    line.map(scoreMap.apply).foldLeft(0L) { case (totalscore, nextscore) => totalscore * 5 + nextscore }
  }

  type Input = List[String]
  def parseInput(): Input = {
    Source
      .fromResource("day10/input.txt")
      .getLines
      .toList
  }

  def partOne(input: Input): Int = {
    val errorMap = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    input.map(line => syntaxCheck(line.toList)).collect { case Left(l) => l }.map(errorMap.apply).sum
  }

  def partTwo(input: Input): Long = {
    val closingLines = input.map(line => syntaxCheck(line.toList)).collect { case Right(r) => r }

    closingLines.map(scoreLine).sorted.apply(closingLines.length / 2)
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}