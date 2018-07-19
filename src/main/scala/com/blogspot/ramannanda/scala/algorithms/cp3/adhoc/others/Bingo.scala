package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.others

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.control.Breaks._

//uva 10813
object Bingo {


  def markIfPresent(grid: Array[Array[Int]], markedArray: Array[Array[Boolean]], j: Int, n: Int) = {
    for (i <- 0 until 5) {
      if (grid(i)(j) == n) {
        markedArray(i)(j) = true
      }
    }
  }

  def checkDiagonals(markedArray: Array[Array[Boolean]]): Boolean = {
    var bingo = true
    breakable {
      for (i <- 0 until 5) {
        if (!markedArray(i)(i)) {
          bingo = false
          break
        }
      }
    }
    if (!bingo) {
      breakable {
        for (i <- 0 until 5) {
          if (!markedArray(i)(4 - i)) {
            bingo = false
            break
          }
        }
      }
    }
    bingo
  }

  def checkRows(markedArray: Array[Array[Boolean]]): Boolean = {
    var bingo = true
    for (i <- 0 until 5) {
      breakable {
        for (j <- 0 until 5) {
          if (!markedArray(i)(j)) {
            bingo = false
            break
          }
          else bingo = true
        }
      }
      if (bingo) return bingo
    }
    bingo
  }

  def checkColumns(markedArray: Array[Array[Boolean]]): Boolean = {
    var bingo = true
    for (j <- 0 until 5) {
      breakable {
        for (i <- 0 until 5) {
          if (!markedArray(i)(j)) {
            bingo = false
            break
          }
          else bingo = true
        }
      }
      if (bingo) return bingo
    }
    bingo
  }

  def checkBingo(grid: Array[Array[Int]], numbers: Seq[Int]): Int = {
    val markedArray = Array.ofDim[Boolean](5, 5)
    markedArray(2)(2) = true
    var numbersSoFar = 0
    for (number <- numbers) {
      if (number <= 15) {
        markIfPresent(grid, markedArray, 0, number)
      }
      else if (number <= 30) {
        markIfPresent(grid, markedArray, 1, number)
      }
      else if (number <= 45) {
        markIfPresent(grid, markedArray, 2, number)
      }
      else if (number <= 60) {
        markIfPresent(grid, markedArray, 3, number)
      }
      else if (number <= 75) {
        markIfPresent(grid, markedArray, 4, number)
      }
      numbersSoFar = numbersSoFar + 1
      if (numbersSoFar >= 4) {
        if (checkDiagonals(markedArray)) return numbersSoFar
        if (checkRows(markedArray)) return numbersSoFar
        if (checkColumns(markedArray)) return numbersSoFar
      }
    }
    numbersSoFar
  }

  def main(args: Array[String]): Unit = {
    val cases = StdIn.readLine().trim.toInt
    for (k <- 0 until cases) {
      val bingoCard = Array.ofDim[Int](5, 5)
      for (i <- 0 until 5) {
        if (i == 2) {
          val thirdRow = StdIn.readLine().split("\\s+").map(_.toInt)
          val thirdRowAc = Array.ofDim[Int](5)
          var k = 0
          for (j <- 0 until 5) {
            if (j == 2) {
              thirdRowAc(j) = Int.MaxValue
              k = k - 1
            } else {
              thirdRowAc(j) = thirdRow(k)
            }
            k = k + 1
          }
          bingoCard(2) = thirdRowAc
        }
        else {
          bingoCard(i) = StdIn.readLine().split("\\s+").map(_.toInt)
        }
      }
      val numbers = new ArrayBuffer[Int]()
      while (numbers.size < 75) {
        numbers ++= StdIn.readLine().split("\\s+").map(_.toInt)
      }
      println(s"BINGO after ${checkBingo(bingoCard, numbers)} numbers announced")
    }
  }
}
