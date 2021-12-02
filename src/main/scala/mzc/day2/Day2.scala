package mzc.day2

import scala.io.Source

object Day2 extends App {
  def partOne(input: List[String]): Int = {
    val result =
      input
        .map { line =>
          line.split(' ').toList match {
            case "forward" :: n :: Nil => (n.toInt, 0)
            case "up" :: n :: Nil => (0, -n.toInt)
            case "down" :: n :: Nil => (0, n.toInt)
          }
        }
        .foldLeft((0,0)) {
          case (acc, next) =>
            (acc._1 + next._1, acc._2 + next._2)
        }

    result._1 * result._2
  }

  def partTwo(input: List[String]): Int = {
    val result =
      input
        .map { line =>
          line.split(' ').toList match {
            case "forward" :: n :: Nil => (n.toInt, 0)
            case "up" :: n :: Nil => (0, -n.toInt)
            case "down" :: n :: Nil => (0, n.toInt)
          }
        }
        .foldLeft((0,0,0)) { case ((horizontal, depth, aim), (nextHorizontal, nextAim)) =>
          val newAim = aim + nextAim
          val newHorizontal = horizontal + nextHorizontal
          val newDepth = depth + aim * nextHorizontal

          (newHorizontal, newDepth, newAim)
        }

    result._1 * result._2
  }

  val input = Source.fromResource("day2/input.txt").getLines.toList

  println(partOne(input))
  println(partTwo(input))
}
