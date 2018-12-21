package com.blogspot.ramannanda.scala.algorithms.medium

object MatrixFlip {
  def matrixScore(A: Array[Array[Int]]): Int = {
    for (i <- A.indices) {
      if (A(i)(0) == 0) {
        for (j <- A(i).indices) {
          A(i)(j) = 1 - A(i)(j)
        }
      }
    }
    for (j <- A(0).indices) {
      var zeros = 0
      for (i <- A.indices) {
        if (A(i)(j) == 0) {
          zeros += 1
        }
      }
      if (zeros > A.length / 2.0) {
        for (i <- A.indices) {
          A(i)(j) = 1 - A(i)(j)
        }
      }
    }
    var result = 0
    for (i <- A.indices) {
      for (j <- A(i).indices) {
        result += A(i)(j).toString.toInt << (A(i).length - j - 1)
      }
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(s"Result is ${matrixScore(Array(Array(0, 0, 1, 1), Array(1, 0, 1, 0), Array(1, 1, 0, 0)))}")
  }
}
