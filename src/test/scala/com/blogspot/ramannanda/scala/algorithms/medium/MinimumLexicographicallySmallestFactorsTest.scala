package com.blogspot.ramannanda.scala.algorithms.medium

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}

class MinimumLexicographicallySmallestFactorsTest extends FlatSpec with Matchers with LazyLogging {
 val n1=12
 val seq1=Seq(2,3,4)
 s"testShortestStepsRequiredToAchieveProduct for $n1 and factors $seq1" should " give " in{
   new MinimumLexicographicallySmallestFactors().shortestStepsRequiredToAchieveProduct(n1,seq1).get should be (Seq(1,3,12))
  }
  val n2=300
  val seq2=Seq(2,3,4,5,6)
  s"testShortestStepsRequiredToAchieveProduct for $n2 and factors $seq2" should " give " in{
    new MinimumLexicographicallySmallestFactors().shortestStepsRequiredToAchieveProduct(n2,seq2).get should be (Seq(1,2,10,50,300))
  }
 val n3=900076
  val seq3=Seq(2,3,4,5,6)
  s"testShortestStepsRequiredToAchieveProduct for $n3 and factors $seq3" should " give " in{
    new MinimumLexicographicallySmallestFactors().shortestStepsRequiredToAchieveProduct(n3,seq3) should be (None)
  }
}
