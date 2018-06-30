package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.chess

import scala.io.StdIn
import scala.util.control.Breaks._


object ChessBoardInFen {

  def prepareBoard(line: String): Array[Array[Char]] = {
    val rows = line.split("/")
    val board = Array.fill(8, 8)('u')
    var i = 0
    for (row <- rows) {
      var j = 0
      for (pieceChar <- row) {
        if (pieceChar.isDigit) {
          j = j + pieceChar.asDigit
        } else {
          board(i)(j) = pieceChar
          j = j + 1
        }
      }
      i += 1
    }
    board
  }

  def fillKingAttacks(board: Array[Array[Char]], r: Int, c: Int): Unit = {
    //upwards
    if (r - 1 >= 0) {
      if (c + 1 < 8 && board(r - 1)(c + 1).equals('u')) {
        board(r - 1)(c + 1) = 'a'
      }
      if (board(r - 1)(c).equals('u')) {
        board(r - 1)(c) = 'a'
      }
      if (c - 1 >= 0 && board(r - 1)(c - 1).equals('u')) {
        board(r - 1)(c - 1) = 'a'
      }
    }
    //sides
    if (c + 1 < 8 && board(r)(c + 1).equals('u')) {
      board(r)(c + 1) = 'a'
    }
    if (c - 1 >= 0 && board(r)(c - 1).equals('u')) {
      board(r)(c - 1) = 'a'
    }
    //down
    if (r + 1 < 8) {
      if (c + 1 < 8 && board(r + 1)(c + 1).equals('u')) {
        board(r + 1)(c + 1) = 'a'
      }
      if (board(r + 1)(c).equals('u')) {
        board(r + 1)(c) = 'a'
      }
      if (c - 1 >= 0 && board(r + 1)(c - 1).equals('u')) {
        board(r + 1)(c - 1) = 'a'
      }
    }
  }

  def fillQueenAttacks(board: Array[Array[Char]], r: Int, c: Int) = {
    fillRowAndColumnAttacked(board, r, c)
    fillDiagonalsAttacked(board, r, c)
  }

  def fillBishopAttacks(board: Array[Array[Char]], r: Int, c: Int) = {
    fillDiagonalsAttacked(board, r, c)
  }

  def fillKnightAttacks(board: Array[Array[Char]], r: Int, c: Int) = {
    if (r - 2 >= 0) {
      if (c + 1 < 8) fillPosnAttacked(board, r - 2, c + 1)
      if (c - 1 >= 0) fillPosnAttacked(board, r - 2, c - 1)
    }
    if (r + 2 < 8) {
      if (c + 1 < 8) fillPosnAttacked(board, r + 2, c + 1)
      if (c - 1 >= 0) fillPosnAttacked(board, r + 2, c - 1)
    }
    if (c - 2 >= 0) {
      if (r + 1 < 8) fillPosnAttacked(board, r + 1, c - 2)
      if (r - 1 >= 0) fillPosnAttacked(board, r - 1, c - 2)
    }
    if (c + 2 < 8) {
      if (r + 1 < 8) fillPosnAttacked(board, r + 1, c + 2)
      if (r - 1 >= 0) fillPosnAttacked(board, r - 1, c + 2)
    }
  }

  def fillPawnAttacks(board: Array[Array[Char]], r: Int, c: Int, black: Boolean) = {
    val toAdd = if (black) 1 else -1
    if ((black && r + toAdd < 8) || (!black && r + toAdd >= 0)) {
      if (c + 1 < 8 && board(r + toAdd)(c + 1).equals('u'))
        board(r + toAdd)(c + 1) = 'a'
      if (c - 1 >= 0 && board(r + toAdd)(c - 1).equals('u'))
        board(r + toAdd)(c - 1) = 'a'
    }
  }


  def fillRookAttacks(board: Array[Array[Char]], r: Int, c: Int) = {
    fillRowAndColumnAttacked(board, r, c)
  }

  def fillPosnAttacked(board: Array[Array[Char]], r: Int, c: Int): Boolean = {
    if (board(r)(c).equals('u')) {
      board(r)(c) = 'a'
    }
    if (board(r)(c).equals('a')) true else false
  }

  def fillRowAndColumnAttacked(board: Array[Array[Char]], r: Int, c: Int): Unit = {
    breakable {
      for (j <- c - 1 to 0 by -1) {
        if (!fillPosnAttacked(board, r, j)) break
      }
    }
    breakable {
      for (j <- c + 1 until 8) {
        if (!fillPosnAttacked(board, r, j)) break
      }
    }
    breakable {
      for (i <- r - 1 to 0 by -1) {
        if (!fillPosnAttacked(board, i, c)) break
      }
    }
    breakable {
      for (i <- r + 1 until 8) {
        if (!fillPosnAttacked(board, i, c)) break
      }
    }
  }

  def fillDiagonalsAttacked(board: Array[Array[Char]], r: Int, c: Int): Unit = {
    var (i, j) = (r - 1, c + 1)
    breakable {
      while (i >= 0 && j < 8) {
        if (!fillPosnAttacked(board, i, j)) break
        i -= 1
        j += 1
      }
    }
    i = r + 1
    j = c - 1
    breakable {
      while (i < 8 && j >= 0) {
        if (!fillPosnAttacked(board, i, j)) break
        i += 1
        j -= 1
      }
    }
    i = r - 1
    j = c - 1
    breakable {
      while (i >= 0 && j >= 0) {
        if (!fillPosnAttacked(board, i, j)) break
        i -= 1
        j -= 1
      }
    }
    i = r + 1
    j = c + 1
    breakable {
      while (i < 8 && j < 8) {
        if (!fillPosnAttacked(board, i, j)) break
        i += 1
        j += 1
      }
    }
  }

  def getNonAttackedEmptyCount(line: String): Int = {
    val board: Array[Array[Char]] = prepareBoard(line)
    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        board(i)(j) match {
          case 'u' | 'a' =>
          case 'k' | 'K' => fillKingAttacks(board, i, j)
          case 'q' | 'Q' => fillQueenAttacks(board, i, j)
          case 'n' | 'N' => fillKnightAttacks(board, i, j)
          case 'r' | 'R' => fillRookAttacks(board, i, j)
          case 'p' => fillPawnAttacks(board, i, j, true)
          case 'P' => fillPawnAttacks(board, i, j, false)
          case 'b' | 'B' => fillBishopAttacks(board, i, j)
        }
      }
    }
    var nonAttacked = 0
    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        board(i)(j) match {
          case 'u' => nonAttacked += 1
          case _: Char =>
        }
      }
    }
    nonAttacked
  }


  def main(args: Array[String]): Unit = {
    while (true) {
      val line = StdIn.readLine()
      if (line.isEmpty) {
        return
      }
      print(getNonAttackedEmptyCount(line))
    }
  }
}
