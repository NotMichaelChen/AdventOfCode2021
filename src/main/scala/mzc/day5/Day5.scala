package mzc.day5

import scala.annotation.tailrec
import scala.io.Source

case class Point(row: Int, column: Int)
case class Line(start: Point, end: Point) {
  def fill: Set[Point] = {
    def fillBetween(start: Int, end: Int): List[Int] = {
      if (start < end) {
        (start to end).toList
      } else {
        (start to end by -1).toList
      }
    }

    val minRow = Math.min(start.row, end.row)
    val maxRow = Math.max(start.row, end.row)
    val minColumn = Math.min(start.column, end.column)
    val maxColumn = Math.max(start.column, end.column)

    if (start.row == end.row) {
      (for {
        column <- minColumn to maxColumn
      } yield Point(start.row, column)).toSet
    } else if (start.column == end.column) {
      (for {
        row <- minRow to maxRow
      } yield Point(row, start.column)).toSet
    } else {
      fillBetween(start.row, end.row).zip(fillBetween(start.column, end.column)).map { case (row, column) =>
        Point(row, column)
      }
        .toSet
    }
  }

  def intersect(other: Line): Set[Point] = {
    val thisPoints = fill
    val otherPoints = other.fill

    thisPoints.intersect(otherPoints)
  }
}

object Day5 extends App {
  def intersectLines(input: List[Line]): Set[Point] = {
    @tailrec
    def loop(lines: List[Line], points: Set[Point]): Set[Point] = lines match {
      case head :: tail =>
        val headPoints = tail.map(head.intersect).foldLeft(Set.empty: Set[Point])((acc, elem) => acc.union(elem))
        loop(tail, headPoints.union(points))
      case _ => points
    }

    loop(input, Set.empty)
  }

  def parseInput(): List[Line] = {
    Source
      .fromResource("day5/input.txt")
      .getLines
      .toList
      .map {
        case s"$x1,$y1 -> $x2,$y2" => Line(Point(y1.toInt, x1.toInt), Point(y2.toInt, x2.toInt))
      }
  }

  def partOne(input: List[Line]): Int = {
    val filteredInput = input.filter { line =>
      line.start.row == line.end.row ||
        line.start.column == line.end.column
    }

    intersectLines(filteredInput).size
  }

  def partTwo(input: List[Line]): Int = {
    intersectLines(input).size
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}