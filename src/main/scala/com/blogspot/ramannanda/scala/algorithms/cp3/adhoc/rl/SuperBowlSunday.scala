package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.rl

import scala.io.StdIn

object SuperBowlSunday {
  def printNumbersIfPossible(sum: Int, diff: Int): Unit = {
    if ((sum + diff) % 2 == 0 && (sum - diff) % 2 == 0) {
      val a = (sum + diff) / 2
      val b = (sum - diff) / 2
      if (a > 0 && b > 0) {
        println(s"$a $b")
        return
      }
    }
    println("impossible")

  }

  def main(args: Array[String]): Unit = {
    val cases = StdIn.readLine().toInt
    for (i <- 0 until cases) {
      val numbers = StdIn.readLine().split("\\s+").map(_.toInt)
      printNumbersIfPossible(numbers(0), numbers(1))
    }
  }
}
