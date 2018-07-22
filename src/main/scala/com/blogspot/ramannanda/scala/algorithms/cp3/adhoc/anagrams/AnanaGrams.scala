package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.anagrams

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.control.Breaks._

//uva 00156
object AnanaGrams {
  def printAnanagrams(dict: Seq[String]): Unit = {
    val wordCnt = mutable.Map[String, Int]()
    val words = ArrayBuffer[String]()
    for (word <- dict) {
      val sortedWord = word.toLowerCase.sorted
      if (wordCnt.contains(sortedWord)) {
        wordCnt.put(sortedWord, 2)
      }
      else {
        wordCnt.put(sortedWord, 1)
      }
    }
    val ananaGrams = wordCnt.filter(_._2 == 1).keys.toSeq
    for (word <- dict) {
      if (ananaGrams.contains(word.toLowerCase.sorted)) {
        words += word
      }
    }
    words.sorted.foreach(w => println(s"$w"))
  }

  def main(args: Array[String]): Unit = {
    val ab = ArrayBuffer[String]()
    breakable {
      while (true) {
        val line = StdIn.readLine().trim
        if (line.isEmpty || line.equals("#")) break
        ab ++= line.split("\\s+")
      }
    }
    printAnanagrams(ab)
  }
}
