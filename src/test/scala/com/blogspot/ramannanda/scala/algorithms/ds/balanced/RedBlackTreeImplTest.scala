package com.blogspot.ramannanda.scala.algorithms.ds.balanced

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, Matchers}

class RedBlackTreeImplTest extends FlatSpec with Matchers with LazyLogging {

  "testInsert" should "after inserting 2,3,4,5,6 be balanced" in {
    val redBlackTree = RedBlackTreeImpl[Int](2)
    redBlackTree.insert(3)
    redBlackTree.insert(4)
    redBlackTree.insert(5)
    redBlackTree.insert(6)
    assert(redBlackTree.searchNode(3).data == 3)
    assert(redBlackTree.searchNode(4).data == 4)
    assert(redBlackTree.searchNode(5).data == 5)
    assert(redBlackTree.rootNode.data == 3, "root node must be 3")
  }

  "testDelete" should "after inserting 2,3,4,5,6 and deleting 4 be balanced" in {
    val redBlackTree = RedBlackTreeImpl[Int](2)
    redBlackTree.insert(3)
    redBlackTree.insert(4)
    redBlackTree.insert(5)
    redBlackTree.insert(6)
    redBlackTree.delete(4)
    assert(redBlackTree.searchNode(4) == null)
    assert(redBlackTree.rootNode.data == 3, "root node must be 3 after deletion")
  }

}
