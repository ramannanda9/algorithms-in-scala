package com.blogspot.ramannanda.scala.algorithms.medium

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}
import TicTacToe._

class TicTacToeTest extends FlatSpec with Matchers with LazyLogging {

  "testFindBestMove" should
    """for board [ ]
        o x o
        o x x
        _ _ _
    should  win by placing at 2,1
    """ in {
    val board = Array(Array('o', 'x', 'o'), Array('o', 'x', 'x'), Array('_', '_', '_'))
    TicTacToe().findBestMove(board) shouldEqual ((2, 1))
  }
  "testFindBestMoveLeastMoves" should
    """for board [ ]
        x o o
        _ x o
        _ _ _
    should  win by placing at 2,2
    """ in {
    val board = Array(Array('x', 'o', 'o'), Array('_', 'x', 'o'), Array('_', '_', '_'))
    TicTacToe().findBestMove(board) shouldEqual ((2, 2))
  }
}
