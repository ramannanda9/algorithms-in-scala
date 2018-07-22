package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.palindrome

import scala.io.StdIn

//uva 00401
object MirroredPalindrome {

  val mirrorMap = Map('A' -> 'A', 'E' -> '3', 'H' -> 'H',
    'I' -> 'I', 'J' -> 'L', 'L' -> 'J', 'M' -> 'M', '0' -> '0',
    'S' -> '2', 'T' -> 'T', 'U' -> 'U', 'V' -> 'V', 'W' -> 'W', 'X' -> 'X',
    'Y' -> 'Y', 'Z' -> '5', '1' -> '1', '2' -> 'S', '3' -> 'E', '5' -> 'Z', '8' -> '8')

  def checkPalindrome(text: String): Boolean = {
    var l = text.length - 1
    val textArray = text.toCharArray
    for (i <- 0 until text.length / 2) {
      if (textArray(i) != textArray(l)) return false
      l = l - 1
    }
    true
  }

  def isMirrored(text: String): Boolean = {
    val tArray = text.toCharArray
    var l = text.length - 1
    for (i <- 0 until tArray.length / 2) {
      if (mirrorMap.contains(tArray(i))) {
        if (mirrorMap(tArray(i)) != tArray(l)) return false
      }
      else {
        if (tArray(i) != tArray(l)) return false
      }
      l = l - 1
    }
    true
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      val line = StdIn.readLine().trim
      if (line.isEmpty) return
      val isPalindrome = checkPalindrome(line)
      val isMirroredString = isMirrored(line)
      if (!isPalindrome && !isMirroredString) {
        println(s"$line is not a palindrome")
      }
      else if (isPalindrome && !isMirroredString) {
        println(s"$line is a regular palindrome")
      }
      else if (!isPalindrome && isMirroredString) {
        println(s"$line is a mirrored string")
      }
      else {
        println(s"$line is a mirrored palindrome")
      }
    }
  }

}
