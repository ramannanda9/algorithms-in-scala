package com.blogspot.ramannanda.scala.algorithms.dp

object MaxSubSetSum {
  def maxSubsetSum(arr: Array[Int]): Int = {
    val n = arr.length - 1
    val dp = Array.ofDim[Int](n + 1)
    dp(n) = arr(n)
    dp(n - 1) = Math.max(arr(n - 1), arr(n))
    var maxSum = Int.MinValue
    for (i <- (n - 2) to 0 by -1) {
      val maxLocal = Math.max(arr(i) + dp(i + 2), dp(i + 1))
      dp(i) = Math.max(arr(i), maxLocal)
      if (dp(i) > maxSum) {
        maxSum = dp(i)
      }
    }
    maxSum
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt

    val arr = stdin.readLine.split(" ").map(_.trim.toInt)
    val res = maxSubsetSum(arr)

    println(res)


  }
}
