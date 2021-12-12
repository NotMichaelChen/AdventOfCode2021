package mzc.util

case class Point(row: Int, column: Int) {
  def tuple: (Int, Int) = (row, column)
}

object Point {
  def apply(tuple: (Int, Int)): Point = Point(tuple._1, tuple._2)
}
