package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.cards

import com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.Card

import scala.util.Try

/**
  * Solution to p10646
  */
object WhatIsTheCard {
  def guessCard(cards: Vector[Card]) = {
    var index = cards.size - 27
    var y = 0
    for (i <- 1 to 3) {
      val cardFace = cards(index).cardFace
      val cardValue = getFaceValue(cardFace)
      y = y + cardValue
      index = index - (10 - cardValue) - 1
    }
    val finalVector = cards.slice(0, index + 1) ++ cards.slice(52 - 26, 52)
    println(s"${finalVector(y - 1).cardFace.toString + finalVector(y - 1).cardSuit.toString}")
  }

  private def getFaceValue(cardFace: Char): Int = {
    Try {
      Integer.parseInt(cardFace.toString)
    }.getOrElse(10)
  }

  def main(args: Array[String]): Unit = {
    val numTestCases = scala.io.StdIn.readLine().trim.toInt
    for (i <- 0 until numTestCases) {
      val line = scala.io.StdIn.readLine()
      if (line.isEmpty) return
      val cards = line.trim.split("\\s+")
      val cardSeq = cards.map(s => Card(s.charAt(0), s.charAt(1))).toVector
      guessCard(cardSeq)
    }
  }
}
