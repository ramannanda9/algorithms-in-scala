package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.rl

import scala.io.StdIn

//uva 00608
object CounterfeitDollar {
  def main(args: Array[String]): Unit = {
    val cases = StdIn.readLine().trim.toInt
    for (i <- 1 to cases) {
      val balancedArray = Array.fill[Char](12)('o')
      for (j <- 1 to 3) {
        val line = StdIn.readLine().split("\\s+")
        line(2) match {
          case "even" =>
            val left = line(0)
            val right = line(1)
            for (c <- left.toCharArray) {
              balancedArray(c - 'A') = 'e'
            }
            for (c <- right.toCharArray) {
              balancedArray(c - 'A') = 'e'
            }
          case "up" | "down" =>
            val left = line(0)
            val right = line(1)
            for (c <- left.toCharArray) {
              if (balancedArray(c - 'A') != 'e') {
                if (line(2).equals("up")) {
                  balancedArray(c - 'A') = 'h'
                }
                else {
                  balancedArray(c - 'A') = 'l'
                }
              }
            }
            for (c <- right.toCharArray) {
              if (balancedArray(c - 'A') != 'e') {
                if (line(2).equals("down")) {
                  balancedArray(c - 'A') = 'h'
                }
                else {
                  balancedArray(c - 'A') = 'l'
                }
              }
            }
        }

      }
      for (i <- balancedArray.indices) {
        balancedArray(i) match {
          case 'h' => println(s"${('A' + i).toChar} is the counterfeit and it is heavy.")
          case 'l' => println(s"${('A' + i).toChar} is the counterfeit and it is light.")
          case _ => 0
        }
      }
    }
  }
}
