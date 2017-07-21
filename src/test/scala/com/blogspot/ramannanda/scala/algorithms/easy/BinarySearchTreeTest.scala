package com.blogspot.ramannanda.scala.algorithms.easy

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Ramandeep Singh on 7/20/17.
  */
class BinarySearchTreeTest extends FlatSpec with Matchers with LazyLogging {
  val data: BinarySearchTree[Int] = BinarySearchTree(30)(20, 40, 50)
  s"testSize " should s" for tree with root root(30) data(20,40,50) should be 4" in {
    data.size should be (4)
  }
  val dataHeight: BinarySearchTree[Int] = BinarySearchTree(30)(20, 40, 50,10,5,4)
  s"testHeight " should s" for tree with root(30) data(20,40,50,10,5,4) should be 4" in {
    dataHeight.height should be (4)
  }


}
