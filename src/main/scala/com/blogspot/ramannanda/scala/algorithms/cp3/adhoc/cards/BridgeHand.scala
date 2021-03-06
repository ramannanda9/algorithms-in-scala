package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.cards

import com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.Card

import scala.util.control.Breaks._

object BridgeHand {

  def evaluateBridgeHand(cards: Array[Card]): Unit = {
    val cardsGrouped = cards.groupBy(_.cardSuit)
    val sumCount = cards.map(cardCounts).sum
    val (ignoreRulesCt, subtractCt) = addSubtractCount(cardsGrouped)
    val finalCount = sumCount - subtractCt + ignoreRulesCt
    if (finalCount < 14) {
      println("PASS")
      return
    }
    if ((finalCount - ignoreRulesCt) >= 16 && areAllGroupsStopped(cardsGrouped)) {
      println("BID NO-TRUMP")
      return
    }

    val cardsSorted = cardsGrouped.map(kv => (kv._1, kv._2.length)).toSeq.sortBy(-_._2)
    println(s"BID ${cardsSorted.head._1}")
  }

  private def cardCounts(card: Card): Int = {
    card.cardFace match {
      case 'A' => 4
      case 'K' => 3
      case 'Q' => 2
      case 'J' => 1
      case _ => 0
    }
  }


  private def addSubtractCount(cardGroups: Map[Char, Array[Card]]): (Int, Int) = {
    var toAdd = 0
    var toSubtract = 0
    for (kv <- cardGroups) {
      for (card <- kv._2) {
        card.cardFace match {
          case 'K' if kv._2.length == 1 => toSubtract = toSubtract + 1
          case 'Q' if kv._2.length <= 2 => toSubtract = toSubtract + 1
          case 'J' if kv._2.length <= 3 => toSubtract = toSubtract + 1
          case _ => ""
        }
      }
      if (kv._2.length == 2) {
        toAdd = toAdd + 1
      }
      else if (kv._2.length == 1) {
        toAdd = toAdd + 1
      }
    }
    toAdd = toAdd + (4 - cardGroups.size)
    (toAdd, toSubtract)
  }

  private def areAllGroupsStopped(cardGroups: Map[Char, Array[Card]]): Boolean = {
    for (kv <- cardGroups) {
      breakable {
        for (card <- kv._2) {
          card.cardFace match {
            case 'A' => break()
            case 'K' if kv._2.length >= 2 => break()
            case 'Q' if kv._2.length >= 3 => break()
            case _ => ""
          }
        }
        return false
      }
    }
    true
  }

  def main(args: Array[String]): Unit = {
    println("Blank line to Exit: ")
    while (true) {
      val line = scala.io.StdIn.readLine()
      if (line.isEmpty) return
      val cards = line.trim.split("\\s+")
      val cardSeq = cards.map(s => Card(s.charAt(0), s.charAt(1)))
      evaluateBridgeHand(cardSeq)
    }
  }

}
