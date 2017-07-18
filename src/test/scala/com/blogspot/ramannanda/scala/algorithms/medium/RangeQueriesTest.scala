package com.blogspot.ramannanda.scala.algorithms.medium

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, FunSuite, Matchers}

/**
  * Created by Ramandeep Singh on 7/18/17.
  */
class RangeQueriesTest extends FlatSpec with Matchers with LazyLogging {
  val dataInt = Array(2, 4, 3, 8, 9, 7, 122)
  val rqMin = new RangeQueries(dataInt)
  val rqMax = new RangeQueries(dataInt, false)
  "testLeft" should "  for p=1 be  2 " in {
    rqMin.left(1) should be(2)
  }
  "testRight for p=1" should " be  3 " in {
    rqMin.right(1) should be(3)
  }

  s"testQuery to find the minimum in range for array ${dataInt.toSeq}" should " for range 1,2 return index 2, item 3 " in {
    dataInt(rqMin.query(1, 0, dataInt.length - 1, 1, 2)) should be(3)
  }
  it should s" for range 1 to ${dataInt.length} return index 0, item 2" in {
    dataInt(rqMin.query(1, 0, dataInt.length - 1, 0, dataInt.length - 1)) should be(2)
  }

  s"testQuery to find the maximum in range for array ${dataInt.toSeq}" should " for range 1,2 return index2, item 4 " in {
    dataInt(rqMax.query(1, 0, dataInt.length - 1, 1, 2)) should be(4)
  }
  it should s" for range 1 to ${dataInt.length} return index 6, item 122" in {
    dataInt(rqMax.query(1, 0, dataInt.length - 1, 0, dataInt.length - 1)) should be(122)
  }

}
