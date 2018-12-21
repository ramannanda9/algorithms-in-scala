package com.blogspot.ramannanda.scala.lc.medium

object AToI {
  def myAtoi(str: String): Int = {
    if (str.isEmpty()) {
      return 0
    }
    val words = str.trim.split("\\s+")
    words(0) match {
      case a if a.matches("[A-Za-z]+") => 0
      case b if b.matches("^([+-]?[0-9]+).*") => val pattern = "^([+-])?([0-9]+).*".r
        val pattern(sign, number) = b
        val numberToUse = number.replaceFirst("^0+(?!$)", "")
        val isNegative = sign != null && sign.equals("-")
        if (numberToUse.length() > 10) {
          if (isNegative) {
            return Int.MinValue
          }
          else {
            return Int.MaxValue
          }
        }
        val result = if (isNegative) -1 * numberToUse.toLong else numberToUse.toLong
        if (result < Int.MinValue) {
          return Int.MinValue
        }
        else if (result > Int.MaxValue) {
          return Int.MaxValue
        }
        else {
          result.toInt
        }
      case _ => 0
    }
  }

  def main(args: Array[String]): Unit = {
    println(s"Result is ${myAtoi("-2147483647")}")
  }
}
