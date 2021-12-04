package mzc.day4

import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {
  @tailrec
  def partOne(drawNumbers: List[Int], boards: List[Board]): Int = {
    val current = drawNumbers.head
    val updatedBoards = boards.map(_.set(current))

    updatedBoards.find(_.isWinner) match {
      case Some(winner) =>
        winner.score(current)
      case None => partOne(drawNumbers.tail, updatedBoards)
    }
  }

  @tailrec
  def partTwo(drawNumbers: List[Int], boards: List[Board]): Int = {
    val current = drawNumbers.head
    val updatedBoards = boards.map(_.set(current))

    updatedBoards match {
      case head :: Nil if head.isWinner => head.score(current)
      case boards => partTwo(drawNumbers.tail, boards.filterNot(_.isWinner))
    }
  }

  val input = Source.fromResource("day4/input.txt").getLines.toList

  val (rawDrawNumbers, rawBoards) =
    input match {
      case drawNumbers :: _ :: xs => (drawNumbers, xs.filter(_.nonEmpty).grouped(5).toList)
    }

  val drawNumbers = rawDrawNumbers.strip().split(',').map(_.toInt).toList
  val boards = rawBoards.map(Board(_))

  println(partOne(drawNumbers, boards))
  println(partTwo(drawNumbers, boards))
}

case class Board(board: Array[Array[Int]], marks: Array[Array[Boolean]]) {
  def set(num: Int): Board = {
    val rawRow = board.indexWhere(r => r.contains(num))
    val newBoardOpt =
      for {
        row <- if (rawRow >= 0) Some(rawRow) else None
        column = board(row).indexOf(num)
      } yield {
        set(row, column)
      }

    newBoardOpt.getOrElse(this)
  }

  // from top-left
  def set(row: Int, column: Int): Board = {
    val updatedMarks = marks.updated(row, marks(row).updated(column, true))
    this.copy(marks = updatedMarks)
  }

  def isWinner: Boolean = {
    marks.exists(_.forall(identity)) ||
    marks.transpose.exists(_.forall(identity))
  }

  // assumes board is a winner
  def score(winningNum: Int): Int = {
    val nums =
      for {
        row <- 0 to 4
        column <- 0 to 4
        if !marks(row)(column)
      } yield {
        board(row)(column)
      }

    nums.sum * winningNum
  }
}

object Board {
  def apply(raw: List[String]): Board = {
    val board = raw.toArray.map(_.strip().split("\\s+").map(_.toInt))
    val marks = Array.ofDim[Boolean](5,5)

    Board(board, marks)
  }
}
