package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.anagrams

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

//uva 00454
object FindAnagrams {

  def anagramPairs(words: Seq[String]): Unit = {
    val anaMap = mutable.LinkedHashMap[String, Seq[String]]()
    for (word <- words) {
      val wordStripped = word.replaceAll("\\s+", "").sorted
      if (anaMap.contains(wordStripped)) {
        anaMap.put(wordStripped, anaMap(wordStripped) :+ word)
      }
      else {
        anaMap.put(wordStripped, Seq(word))
      }
    }
    anaMap.foreach(kv => {
      val kvSet = kv._2.toSet
      for (i <- 0 until kvSet.size) {
        for (j <- i + 1 until kvSet.size) {
          println(s"${kv._2(i)} = ${kv._2(j)}")
        }
      }
    })
  }

  def main(args: Array[String]): Unit = {
    var lb = ListBuffer[String]()
    val cases = StdIn.readLine().trim.toInt
    var i = 0
    while (i < cases) {
      val word = StdIn.readLine()
      if (word.isEmpty) {
        anagramPairs(lb)
        i += 1
      }
      lb += word
    }
  }
}
