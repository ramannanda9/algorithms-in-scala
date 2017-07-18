package com.blogspot.ramannanda.scala.algorithms.easy

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, FunSuite, Matchers}

/**
  * Created by Ramandeep Singh on 7/17/17.
  */
class SumIntegerPairs$Test extends FlatSpec with Matchers with LazyLogging {
  val data = Array(1, 5, 4, 9, 8, 10)
  s"testFindPairForSum for array ${data.toSeq}" should "find (1,4) as the pair for getting 5" in {
    SumIntegerPairs.findPairForSum(data,5) should be ((1,4))
  }
   it should "find (1,8) as the pair for getting 9" in {
    SumIntegerPairs.findPairForSum(data,9) should be ((1,8))
  }
  it should "throw an RuntimeException for getting 4" in {
    a[RuntimeException] should be thrownBy SumIntegerPairs.findPairForSum(data,4)
  }

}
