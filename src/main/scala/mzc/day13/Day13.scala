package mzc.day13

import mzc.util.Point

import scala.annotation.tailrec
import scala.io.Source

case class Fold(axis: String, line: Int)

object Day13 extends App {
  @tailrec
  def foldPoints(points: Set[Point], folds: List[Fold]): Set[Point] = {
    if (folds.isEmpty)
      return points

    val fold = folds.head

    val newPoints = points.map { point =>
      fold match {
        case Fold("y", line) =>
          if (point.row < line)
            point
          else
            Point(point.row - 2 * (point.row - line), point.column)
        case Fold("x", line) =>
          if (point.column < line) {
            point
          } else {
            Point(point.row, point.column - 2 * (point.column - line))
          }
      }
    }

    foldPoints(newPoints, folds.tail)
  }

  def printPoints(points: List[Point]): Unit = {
    val lines = points.sortBy(_.tuple).groupBy(_.row).toList.sortBy(_._1).map(_._2.map(_.column).sliding(2).map { case first :: second :: Nil => second - first }.toList)

    lines.foreach { line =>
      line.foreach { padding =>
        print("█")
        print(" ".repeat(padding-1))
      }
      print("█")
      println()
    }
  }

  type Input = (List[Point], List[Fold])

  def parseInput(): Input = {
    Source
      .fromResource("day13/input.txt")
      .getLines
      .toList
      .flatMap {
        case s"$column,$row" => Some(Left(Point(row.toInt, column.toInt)))
        case s"fold along $axis=$line" => Some(Right(Fold(axis, line.toInt)))
        case _ => None
      }
      .partitionMap(identity)
  }

  def partOne(input: Input): Int = {
    val (points, folds) = input
    foldPoints(points.toSet, List(folds.head)).size
  }

  def partTwo(input: Input): Unit = {
    val (points, folds) = input

    val foldedPoints = foldPoints(points.toSet, folds)

    printPoints(foldedPoints.toList)
  }

  val input = parseInput()

  println(partOne(input))
  partTwo(input)
}