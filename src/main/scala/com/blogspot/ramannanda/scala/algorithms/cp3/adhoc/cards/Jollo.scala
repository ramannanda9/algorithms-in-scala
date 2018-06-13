package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.cards

/** uva 12247 **/
object Jollo {
  def getMinCard(cardToGetHigherFrom: Int, availCards: Set[Int]): Unit = {
    var i = cardToGetHigherFrom + 1
    while (i <= 52) {
      if (availCards.contains(i)) {
        println(i)
        return
      }
      i = i + 1
    }
    println(-1)
  }

  def areTwoLess(): Unit = {

  }

  def main(args: Array[String]): Unit = {
    val cardsSet = (1 to 52).toSet
    while (true) {
      val input = scala.io.StdIn.readLine()
      val cards = input.split("\\s+").map(_.toInt).toIndexedSeq
      if (cards.head.equals(0)) {
        return
      }
      val princeCards: Seq[Int] = cards.slice(3, 5).sorted
      val princessCards = cards.slice(0, 3).sorted
      if (princeCards.head < princessCards(1) && princeCards(1) < princessCards(2)) {
        println(-1)
      } else {
        val availCards = cardsSet -- cards
        getMinCard(princessCards(1), availCards)
      }

    }
  }
}
