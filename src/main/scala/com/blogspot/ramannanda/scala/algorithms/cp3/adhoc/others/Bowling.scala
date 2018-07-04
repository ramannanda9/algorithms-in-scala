package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.others

//UVA-00584
object Bowling {

  def countFrame(scores: Array[String], i: Int): Int = {
    scores(i) match {
      case a if a.charAt(0).isDigit =>
        if (scores(i - 1)(0).isDigit) 1 else 0
      case "/" | "X" => 1
    }
  }

  def calculateScore(scores: Array[String]): Int = {
    def matchScore(scores: Array[String], i: Int, isBonus: Boolean): Int = {
      var score = 0
      scores(i) match {
        case a if a.charAt(0).isDigit =>
          score += a.toInt
          score
        case "/" => {
          //since we added this earlier, when we should have not
          score += 10 - matchScore(scores, i - 1, false)
          //add next one without bonus
          if (isBonus) {
            score += matchScore(scores, i + 1, false)
          }
          score
        }
        case "X" => {
          score += 10
          if (isBonus) {
            //add next two without bonus
            for (j <- 1 to 2) {
              score += matchScore(scores, i + j, false)
            }
          }
          score
        }
      }
    }

    var score = 0
    var i = 0
    var frame = 0
    while (i < scores.size && frame <= 10) {
      if (i > 0) {
        frame += countFrame(scores, i)
      }
      score += matchScore(scores, i, true)
      i += 1
    }
    score
  }


  def main(args: Array[String]): Unit = {
    while (true) {
      val input = scala.io.StdIn.readLine()
      if (input.trim.equals("Game Over")) return
      val inputArray = input.split("\\s+")
      println(calculateScore(inputArray))
    }
  }

}
