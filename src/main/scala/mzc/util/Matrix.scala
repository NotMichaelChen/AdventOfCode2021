package mzc.util

case class Matrix[A](matrix: Vector[Vector[A]]) {
  def map[B](f: A => B): Matrix[B] = {
    this.copy(matrix = matrix.map(_.map(f)))
  }

  def mapIndices[B](f: Point => B): Matrix[B] = {
    val newMatrix = matrix.indices.map { row =>
      matrix(row).indices.map { column =>
        val point = Point(row, column)
        f(point)
      }
        .toVector
    }
      .toVector

    this.copy(matrix = newMatrix)
  }

  def exists(f: A => Boolean): Boolean = {
    matrix.exists(_.exists(f))
  }

  def count(f: A => Boolean): Int = {
    matrix.map(_.count(f)).sum
  }

  def get(point: Point): A = matrix(point.row)(point.column)

  def getOpt(point: Point): Option[A] = {
    for {
      rowValue <- matrix.lift(point.row)
      columnValue <- rowValue.lift(point.column)
    } yield columnValue
  }

  // TODO: copy-pasted from earlier solution, should simplify
  def cardinalAdjacentIndices(point: Point): Vector[Point] = {
    val (row, column) = point.tuple

    val rows = Vector(row, row, row - 1, row + 1)
    val columns = Vector(column - 1, column + 1, column, column)

    rows.zip(columns).map(Point.apply).filter(p => getOpt(p).isDefined)
  }

  def adjacentIndices(point: Point): Vector[Point] = {
    val (row, column) = point.tuple

    val adjacentPoints =
      for {
        rowIndex <- row - 1 to row + 1
        columnIndex <- column - 1 to column + 1
        candidatePoint = Point(rowIndex, columnIndex)
        if getOpt(candidatePoint).isDefined && candidatePoint != point
      } yield {
        candidatePoint
      }

    adjacentPoints.toVector
  }
}
