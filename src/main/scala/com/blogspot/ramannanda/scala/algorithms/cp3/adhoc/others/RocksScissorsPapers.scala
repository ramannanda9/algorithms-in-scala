package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.others

import scala.annotation.tailrec
import scala.io.StdIn

//uva 10443
object RocksScissorsPapers {

  def checkNeighbors(o: Char, c: Char, grid: Array[Array[Char]],
                     i: Int, j: Int,
                     row: Int, col: Int): Char = {
    if (i - 1 >= 0) {
      if (grid(i - 1)(j) == c) {
        return c
      }
    }
    if (j - 1 >= 0) {
      if (grid(i)(j-1) == c) {
        return c
      }
    }
    if (i + 1 < row) {
      if (grid(i + 1)(j) == c) {
        return c
      }
    }
    if (j + 1 < col) {
      if (grid(i)(j + 1) == c) {
        return c
      }
    }
    o
  }

  @tailrec
  final def performIteration(grid: Array[Array[Char]], n: Int, r: Int, c: Int): Array[Array[Char]] = {
    val todayArray = Array.ofDim[Char](r, c)
    if (n <= 0) return grid
    for (i <- 0 until r) {
      for (j <- 0 until c) {
        grid(i)(j) match {
          case 'R' => todayArray(i)(j) = checkNeighbors('R', 'P', grid, i, j, r, c)
          case 'P' => todayArray(i)(j) = checkNeighbors('P', 'S', grid, i, j, r, c)
          case 'S' => todayArray(i)(j) = checkNeighbors('S', 'R', grid, i, j, r, c)
        }
      }
    }
    performIteration(todayArray, n - 1, r, c)
  }

  def main(args: Array[String]): Unit = {
    val cases = StdIn.readLine().trim.toInt
    for (k <- 0 until cases) {
      val inputs = StdIn.readLine().split("\\s+").map(_.toInt)
      val r = inputs(0)
      val c = inputs(1)
      val n = inputs(2)
      var grid = Array.ofDim[Char](r, c)
      for (i <- 0 until r) {
        grid(i) = StdIn.readLine().toCharArray
      }
      grid=performIteration(grid, n, r, c)
      for (i <- 0 until r) {
        for (j <- 0 until c) {
          print(grid(i)(j))
        }
        println()
      }
    }
  }

}
