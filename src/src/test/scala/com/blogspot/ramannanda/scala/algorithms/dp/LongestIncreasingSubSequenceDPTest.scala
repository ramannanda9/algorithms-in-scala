package com.blogspot.ramannanda.scala.algorithms.dp

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Ramandeep Singh on 7/17/17.
  */
class LongestIncreasingSubSequenceDPTest extends FlatSpec with Matchers with LazyLogging {

  val longestIncreasingSubSequenceDP = new LongestIncreasingSubSequenceDP[Int]()
  val data = Array(-7, 10, 9, 2, 3, 8, 8, 1)

  s"getLICS for array ${data.toSeq}" should "find maximum sequence to be -7,2,3,8 " in {
    longestIncreasingSubSequenceDP.getLICS(data) should be(Vector(-7, 2, 3, 8))
  }

}
