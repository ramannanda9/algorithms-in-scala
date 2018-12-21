package com.blogspot.ramannanda.scala.lc.medium

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object FindAndReplace {
  def findAndReplacePattern(words: Array[String], pattern: String): List[String] = {
    val lb = ListBuffer[String]()
    for (word <- words) {
      val hm = mutable.HashMap[Char, Char]()
      val hs = mutable.HashSet[Char]()
      val sb = new StringBuilder()
      breakable {
        for (i <- 0 until pattern.length) {
          if (hm.contains(pattern.charAt(i))) {
            sb.append(hm(pattern.charAt(i)))
          }
          else {
            if (!hs.contains(word.charAt(i))) {
              hm.put(pattern.charAt(i), word.charAt(i))
              hs.add(word.charAt(i))
              sb.append(word.charAt(i))
            }
            else {
              sb.append(pattern.charAt(i))
            }
          }
          if (!sb.charAt(i).equals(word.charAt(i))) break()
        }
        lb += word
      }
    }
    lb.toList
  }

  def main(args: Array[String]): Unit = {
    println(findAndReplacePattern(Array("abc", "deq", "mee", "aqq", "dkd", "ccc"), "abb"))
  }
}
