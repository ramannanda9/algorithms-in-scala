package com.blogspot.ramannanda.scala.algorithms.cp3

package object adhoc {

  case class Card(cardFace: Char, cardSuit: Char)

  sealed trait ChessPiece

  case object King extends ChessPiece

  case object Queen extends ChessPiece

  case object Rook extends ChessPiece

  case object Pawn extends ChessPiece

  case object Knight extends ChessPiece

}
