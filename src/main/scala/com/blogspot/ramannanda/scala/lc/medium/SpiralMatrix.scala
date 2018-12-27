package com.blogspot.ramannanda.scala.lc.medium

object SpiralMatrix {
  def isValid(i: Int, j: Int, r: Int, c: Int): Boolean = {
    i >= 0 && j >= 0 && i < r && j < c
  }

  def spiralMatrixIII(R: Int, C: Int, r0: Int, c0: Int): Array[Array[Int]] = {
    var count = 0
    val arr = Array.ofDim[Int](R * C, 2)
    var numInc = 1
    val dirs = Array((0, 1), (1, 0), (0, -1), (-1, 0))
    var dir = 0
    var i = r0
    var j = c0
    arr(count) = Array(i, j)
    count += 1
    while (count < R * C) {
      for (k <- 0 until numInc) {
        i += dirs(dir)._1
        j += dirs(dir)._2
        if (isValid(i, j, R, C)) {
          arr(count) = Array(i, j)
          count += 1
        }
      }
      dir = (dir + 1) % 4
      for (k <- 0 until numInc) {
        i += dirs(dir)._1
        j += dirs(dir)._2
        if (isValid(i, j, R, C)) {
          arr(count) = Array(i, j)
          count += 1
        }
      }
      dir = (dir + 1) % 4
      numInc += 1
    }
    arr
  }
}
