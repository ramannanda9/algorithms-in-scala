package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.rl

import scala.io.StdIn


//uva 00637
object BookletPrinting {


  def printSheet(sheet: Int, pages: Int, values: (Int, Int), isFront: Boolean = false): Unit = {
    if (isFront) {
      println(s"Sheet $sheet, front: ${if (values._1 > pages) "Blank" else values._1}, " +
        s"${values._2}")
    }
    else {
      println(s"Sheet $sheet, back: ${values._1}, " +
        s"${if (values._2 > pages) "Blank" else values._2}")
    }
  }

  def printBook(pages: Int): Unit = {
    if (pages == 1) {
      println(s"Sheet 1, front: Blank, 1)")
    } else {
      val sheet = Math.ceil(pages * 1.0 / 4).toInt
      var a = 1
      var b = sheet * 4
      for (i <- 1 to sheet) {
        printSheet(i, pages, (b, a), true)
        b = b - 1; a = a + 1
        printSheet(i, pages, (a, b), false)
        b = b - 1; a = a + 1
      }
    }


  }

  def main(args: Array[String]): Unit = {
    while (true) {
      val pages = StdIn.readLine().trim.toInt
      if (pages == 0) return
      printBook(pages)
    }
  }
}
