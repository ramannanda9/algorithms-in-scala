package com.blogspot.ramannanda.scala.algorithms.easy

import scala.annotation.tailrec

/**
  * Created by Ramandeep Singh on 7/17/17.
  */
object SumIntegerPairs {

  /**
    * This method finds a pair which sums to sum
    *
    * @param data the array
    * @param sum  the sum to add up to
    * @return the pair
    */
  def findPairForSum(data: Array[Int], sum: Int): (Int, Int) = {
    val sorted = data.sorted
    @tailrec def findPairForSumRec(data: Array[Int], sum: Int): (Int, Int) = {
      val end = data.length - 1
      val start = 0
      if (start == end) {
        throw new RuntimeException("Sum Not Possible using pairs")
      }
      val x = data(start)
      val y = data(end)
      if (x + y == sum) {
        (x, y)
      }
      else if (x + y < sum) {
        findPairForSumRec(data.slice(start + 1, end), sum)
      }
      else {
        findPairForSumRec(data.slice(start, end - 1), sum)
      }
    }
    findPairForSumRec(sorted, sum)
  }

}
