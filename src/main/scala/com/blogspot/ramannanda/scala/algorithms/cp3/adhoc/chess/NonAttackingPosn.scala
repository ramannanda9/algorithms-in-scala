package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.chess

import scala.io.StdIn

//uva 278
object NonAttackingPosn {

  def numMoves(piece: Char, nCols: Int, nRows: Int): Int = {
    piece match {
      case 'r' => Math.min(nRows, nCols)
      case 'Q' => Math.min(nRows, nCols)
      case 'k' => (nCols * nRows + 1) / 2 //place them on all white or black
      case 'K' => (nCols + 1) / 2 * (nRows + 1) / 2 //every alternate row and column is safe

    }
  }

    def main(args: Array[String]): Unit = {
      val numCases = StdIn.readLine().trim.toInt
      for (i <- 0 until numCases) {
        val input = StdIn.readLine().split("\\s+")
        println(numMoves(input(0).charAt(0), input(1).toInt, input(2).toInt))
      }
    }

}
