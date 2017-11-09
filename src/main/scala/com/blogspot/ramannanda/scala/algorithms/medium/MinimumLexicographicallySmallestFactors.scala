package com.blogspot.ramannanda.scala.algorithms.medium

import scala.annotation.tailrec

/**
  * The idea behind this problem is that you have to find the minimum steps to reach a product
  * One needs to determine whether the solution is possible given a set of factors
  */
class MinimumLexicographicallySmallestFactors {
  //tracks global steps
  private var minSteps = Integer.MAX_VALUE
  //the bestSolution so far
  private var bestSolution = Seq[Int]()

  /**
    * Compares two sequences lexicographically. Assumes they are sorted
    *
    * @param first the first sequence
    * @param other the second sequence
    * @param ev    the implicit evidence for ordering
    * @tparam K the type of Sequence
    * @return true when first>second else false.
    */
  @tailrec
  private def compare[K](first: Seq[K], other: Seq[K])(implicit ev: Ordering[K]): Boolean = {
    import ev.mkOrderingOps
    if (first.size != other.size) {
      first.size > other.size
    }
    else {
      if (first.nonEmpty) {
        if (!first.head.equals(other.head)) {
          first.head > other.head
        } else {
          compare(first.tail, other.tail)
        }
      }
      else {
        true
      }
    }

  }

  /**
    *
    * @param numbers     the numbers to consider
    * @param n           the product to achieve
    * @param numbersUsed the numbersUsed to reach the product
    * @param numSteps    the number of steps take so far
    * @param totalSoFar  the total product so far
    * @return
    */
  private def factorize(numbers: Seq[Int], n: Int, numbersUsed: Seq[Int] = Seq[Int](), numSteps: Int = 0, totalSoFar: Int = 1): Int = {
    //number exceeded limit for integer, so this number is not part of solution
    if (totalSoFar < 0) {
      return -1
    }
    //pruning
    if (numSteps > minSteps) {
      return 1
    }
    if (totalSoFar > n) {
      return -1
    }
    if (totalSoFar == n) {
      if (numSteps == minSteps) {
        if (compare(bestSolution, numbersUsed.sorted)) {
          bestSolution = numbersUsed.sorted
        }
      }
      else if (numSteps < minSteps) {
        bestSolution = numbersUsed.sorted
        minSteps = numSteps
      }
    }
    if (numbers.isEmpty) {
      return 1
    }
    val number: Long = numbers.head * totalSoFar
    //number is going to exceed the limit so skip
    if (number > n || number > Int.MaxValue) {
      return -1
    }
    factorize(numbers, n, numbersUsed ++ Seq(number.toInt), numSteps + 1, number.toInt)
    factorize(numbers.tail, n, numbersUsed, numSteps, totalSoFar)

  }

  /**
    * Checks whether the product is possible at all, given a set of numbers
    *
    * @param n            the number to achieve
    * @param numbersToTry the sequence of numbers to try
    * @return true if product is achievable else false.
    */
  private def areFactorsPresent(n: Int, numbersToTry: Seq[Int]): Boolean = {
    if (n == 1) {
      true
    }
    else if (numbersToTry.nonEmpty) {
      val head = numbersToTry.head
      if (n % head == 0) {
        areFactorsPresent(n / head, numbersToTry)
      }
      else {
        areFactorsPresent(n, numbersToTry.tail)
      }
    }
    else {
      false
    }
  }

  /**
    * Gives the steps required
    *
    * @param n       the number
    * @param factors the factors to consider
    * @return the factors if product is achievable or none.
    */
  def shortestStepsRequiredToAchieveProduct(n: Int, factors: Seq[Int]): Option[Seq[Int]] = {
    minSteps = Integer.MAX_VALUE
    //the bestSolution so far
    bestSolution = Seq[Int]()
    //The filtered list of numbers to use
    if (areFactorsPresent(n, factors)) {
      if (factorize(factors, n) == 1) {
        val result = Seq(1) ++ bestSolution.slice(0, bestSolution.size - 1) ++ Seq(n)
        return Some(result)
      }
    }
    None

  }


}
