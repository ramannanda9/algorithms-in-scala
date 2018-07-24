package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.anagrams

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

//uva 00195
object Permutations {

  def permute(word: String): Seq[String] = {
    def permuteRec(prefix: String, word: String): Seq[String] = {
      if (word.length == 1) {
        Seq(prefix + word)
      }
      else {
        var lb = ListBuffer[String]()
        for (i <- 0 until word.length) {
          lb ++= permuteRec(prefix + word.charAt(i),
            word.substring(0, i) + word.substring(i + 1, word.length))
        }
        lb
      }
    }

    permuteRec("", word)
  }

  def main(args: Array[String]): Unit = {
    val cases = StdIn.readLine().trim.toInt
    for (i <- 0 until cases) {
      val word = StdIn.readLine()
      permute(word).sorted.toSet.foreach(println)
    }
  }
}
