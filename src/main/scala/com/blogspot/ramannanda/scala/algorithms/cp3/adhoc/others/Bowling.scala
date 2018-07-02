package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.others
//UVA-00584
object Bowling {

  def calculateScore(scores: Array[String]): Int = {
    def matchScore(scores: Array[String], i: Int, frame: Int, isBonus: Boolean): (Int, Int) = {
      var score = 0
      var index = i
      scores(i) match {
        case a if a.charAt(0).isDigit =>
          score += a.toInt
          if (i > 0 && scores(i - 1).charAt(0).isDigit) {
            (score, frame + 1)
          }
          else (score, frame)
        case "/" => {
          //since we added this earlier, when we should have not
          score += 10 - matchScore(scores, i - 1, frame, false)._1
          //add next one without bonus
          if (isBonus) {
            val scoreTup = matchScore(scores, i + 1, frame + 1, false)
            score += scoreTup._1
          }
          (score, frame + 1)
        }
        case "X" => {
          score += 10
          if (isBonus) {
            //add next two without bonus
            for (j <- 1 to 2) {
              val scoreTup = matchScore(scores, i + j, frame + 1, false)
              score += scoreTup._1
            }
          }
          (score, frame + 1)
        }
      }
    }

    var score = 0
    var i = 0
    var frame = 0
    while (i < scores.size && frame <= 10) {
      val scoreTup = matchScore(scores, i, frame, true)
      frame = scoreTup._2
      score += scoreTup._1
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
