package com.blogspot.ramannanda.scala.algorithms.medium

case class TicTacToe(aiSymbol: Char = 'x', playerSymbol: Char = 'o') {

  type Board = Array[Array[Char]]

  def getScore(board: Board, depth: Int): Int = {
    for (row <- 0 until 3) {
      val score = checkThreeInSeq(board(row))
      if (score != 0) {
        if (score > 0) return score - depth else score + depth
      }
    }
    for (col <- 0 until 3) {
      val score = checkThreeInSeq(board.map(_ (col)))
      if (score != 0) {
        if (score > 0) return score - depth else score + depth
      }
    }
    val score = checkDiagonals(board)
    if (score != 0) {
      if (score > 0) return score - depth else return score + depth
    }
    0

  }

  def isMovesLeft(board: Board): Boolean = {
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (board(i)(j).equals('_')) return true
      }
    }
    false
  }

  def checkDiagonals(board: Board): Int = {
    if (board(0)(0).equals(board(1)(1)) && board(1)(1).equals(board(2)(2))) {
      if (board(0)(0).equals(aiSymbol)) return 10 else return -10
    }
    if (board(0)(2).equals(board(1)(1)) && board(1)(1).equals(board(2)(0))) {
      if (board(0)(2).equals(aiSymbol)) return 10 else return -10
    }
    0
  }

  def checkThreeInSeq(arr: Array[Char]): Int = {
    var count = 1
    var prevSymbol = arr(0)
    for (i <- 1 until arr.length) {
      if (arr(i).equals(prevSymbol)) {
        count = count + 1
        if (count == 3) {
          if (arr(i).equals(aiSymbol)) return 10 else return -10
        }
      }
      else {
        count = 1
        prevSymbol = arr(i)
      }
    }
    0
  }


  def minimax(board: Board, depth: Int, isMax: Boolean, alpha: Int, beta: Int): Int = {
    var alphaVar = alpha
    var betaVar = beta
    val score = getScore(board, depth)
    if (score != 0) {
      return score
    }
    if (!isMovesLeft(board)) return 0
    if (isMax) {
      var best = -100
      for (i <- 0 until 3) {
        for (j <- 0 until 3) {
          if (board(i)(j).equals('_')) {
            board(i)(j) = aiSymbol
            best = Math.max(best, minimax(board, depth + 1, false, alphaVar, betaVar))
            alphaVar = Math.max(best, alphaVar)
            board(i)(j) = '_'
            if (betaVar < alphaVar) return best
          }
        }
      }
      best
    }
    else {
      var best = 100
      for (i <- 0 until 3) {
        for (j <- 0 until 3) {
          if (board(i)(j).equals('_')) {
            board(i)(j) = playerSymbol
            best = Math.min(best, minimax(board, depth + 1, false, alphaVar, betaVar))
            betaVar = Math.min(betaVar, best)
            board(i)(j) = '_'
            if (betaVar < alphaVar) return best
          }
        }
      }
      best
    }
  }

  def findBestMove(board: Board): (Int, Int) = {
    var bestScore = -100
    var bestMove = (0, 0)
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (board(i)(j).equals('_')) {
          board(i)(j) = aiSymbol
          val scoreVal = minimax(board, 0, false,-100,100)
          board(i)(j) = '_'
          if (scoreVal > bestScore) {
            bestScore = scoreVal
            bestMove = (i, j)
          }
        }
      }
    }
    bestMove

  }

}
