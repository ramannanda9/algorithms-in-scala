package com.blogspot.ramannanda.scala.algorithms.easy

import com.blogspot.ramannanda.scala.algorithms.ds.{BinaryTree, EmptyBinaryTree, Node}
import com.typesafe.scalalogging.LazyLogging


/**
  * Created by Ramandeep Singh on 7/19/17.
  */
class BinarySearchTree[V](val rootNode: Node[V])(implicit ev: Ordering[V]) extends LazyLogging {

  import ev.mkOrderingOps

  def add(parent: BinaryTree[V], data: V): Node[V] = parent match {
    case node: Node[V] => {
      if (data > node.data) {
        node.r match {
          case child: Node[V] => add(child, data)
          case EmptyBinaryTree => {
            val child = Node(data, EmptyBinaryTree, EmptyBinaryTree, node)
            node.r = child
            child
          }
        }
      }
      else {
        node.l match {
          case child: Node[V] => add(child, data)
          case EmptyBinaryTree => {
            val child = Node(data, EmptyBinaryTree, EmptyBinaryTree, node)
            node.l = child
            child
          }
        }
      }
    }
    case EmptyBinaryTree =>
      Node[V](data, EmptyBinaryTree, EmptyBinaryTree, null)
  }

  def size: Int = {
    rootNode.size
  }

  def height: Int = {
    rootNode.height
  }
}

object BinarySearchTree extends LazyLogging {

  def apply[V](rootData: V)(childrenData: V*)(implicit ev: Ordering[V]): BinarySearchTree[V] = {
    val root = Node(rootData, EmptyBinaryTree, EmptyBinaryTree, null)
    val bst = new BinarySearchTree[V](root)
    logger.info(s"root added ${root.data}")
    for (childData <- childrenData) {
      val child = bst.add(root, childData)
      logger.info(s"Child Added ${child.data}")
    }
    bst
  }
}


