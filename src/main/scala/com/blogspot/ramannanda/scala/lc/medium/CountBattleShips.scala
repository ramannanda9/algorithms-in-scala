package com.blogspot.ramannanda.scala.lc.medium

import scala.util.control.Breaks._

object CountBattleShips {
  def countBattleships(board: Array[Array[Char]]): Int = {
    val m = board.length
    if (m == 0) return 0
    val n = board(0).length
    var count = 0
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        breakable {
          if (board(i)(j) == '.') break
          if (i > 0 && board(i - 1)(j) == 'X') break
          if (j > 0 && board(i)(j - 1) == 'X') break
          count += 1
        }
      }
    }
    count
  }

  def main(args: Array[String]): Unit = {
    countBattleships(Array(Array('X', '.', '.', 'X'), Array('.', '.', '.', 'X'), Array('.', '.', '.', 'X')))
  }
}
