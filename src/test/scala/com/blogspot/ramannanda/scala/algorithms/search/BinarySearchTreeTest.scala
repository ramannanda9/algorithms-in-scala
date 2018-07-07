package com.blogspot.ramannanda.scala.algorithms.search

import com.blogspot.ramannanda.scala.algorithms.ds.{BinaryTree, Node}
import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Ramandeep Singh on 7/20/17.
  */
class BinarySearchTreeTest extends FlatSpec with Matchers with LazyLogging {
  val data: BinarySearchTree[Int] = BinarySearchTree(30)(20, 40, 50)
  s"testSize " should s" for tree with root root(30) data(20,40,50) should be 4" in {
    data.size should be(4)
  }
  val dataHeight: BinarySearchTree[Int] = BinarySearchTree(30)(20, 40, 50, 10, 5, 4)
  s"testHeight " should s" for tree with root(30) data(20,40,50,10,5,4) should be 4" in {
    dataHeight.height should be(4)
  }
  val dataSumTest: BinarySearchTree[Int] = BinarySearchTree(30)(20, 40, 50, 10, 5, 4)
  s"testSumAtLevels " should s" for tree with root(30) data(20,40,50,10,5,4) be (30,60,60,5,4)" in {
    dataSumTest.sumAtLevels[Int](dataSumTest.rootNode) should be(Seq(30, 60, 60, 5, 4))
  }
  val dataTraversal: BinarySearchTree[Int] = BinarySearchTree(30)(20, 40, 50, 10, 5, 4)
  s"testInOrder " should s" for tree with root(30) data(20,40,50,10,5,4) be (4)(5)(10)(20)(30)(40)(50)" in {
    dataTraversal.inOrder should be("(4)(5)(10)(20)(30)(40)(50)")
  }
  s"testPreOrder " should s" for tree with root(30) data(20,40,50,10,5,4) be (30)(20)(10)(5)(4)(40)(50)" in {
    dataTraversal.preOrder should be("(30)(20)(10)(5)(4)(40)(50)")
  }
  s"testPostOrder " should s" for tree with root(30) data(20,40,50,10,5,4) be (4)(5)(10)(20)(50)(40)(30)" in {
    dataTraversal.postOrder should be("(4)(5)(10)(20)(50)(40)(30)")
  }
  val dataMirror: BinarySearchTree[Int] = BinarySearchTree(30)(20, 40, 50, 10, 5, 4)
  s"After Mirroring the tree and inOrder for " should s" for tree with root(30) data data(20,40,50,10,5,4) be (50)(40)(30)(20)(10)(5)(4) be " in {
    dataMirror.mirror match {
      case node: Node[Int] => BinaryTree.inOrder(node) should be("(50)(40)(30)(20)(10)(5)(4)")
      case _ =>
    }
  }


}
