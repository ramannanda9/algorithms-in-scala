package com.blogspot.ramannanda.scala.algorithms.easy

import com.blogspot.ramannanda.scala.algorithms.ds.{BinaryTree, EmptyBinaryTree, Node}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.{Map, Queue}


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

  /**
    * This function sums the values at level i in tree
    *
    * @param rootNode the rootNode of the tree
    * @param ev       the view bound constraint that the type should be numeric
    * @tparam D the DataType should be a Numeric data type
    * @return the Array of sum of nodes.
    */
  def sumAtLevels[D](rootNode: Node[D])(implicit ev: Numeric[D]): Seq[D] = {

    def augmentedBFS(rootNode: Node[D]): Seq[D] = {
      val levelsMap = Map[Node[D], Int](rootNode -> 0)
      val levelsSumMap = Map[Int, D](0 -> rootNode.data)
      val queue = Queue[Node[D]](rootNode)
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        sumAtLevel(node, child = node.l, levelsMap, levelsSumMap, queue)
        sumAtLevel(node, child = node.r, levelsMap, levelsSumMap, queue)
      }
      levelsSumMap.toSeq.sortBy(_._1).map(_._2)
    }
    augmentedBFS(rootNode)
  }

  def inOrder: String = {
    BinaryTree.inOrder(rootNode)
  }

  def preOrder: String = {
    BinaryTree.preOrder(rootNode)
  }

  def postOrder: String = {
    BinaryTree.postOrder(rootNode)
  }

  def mirror: BinaryTree[V] = {
    BinaryTree.mirror(rootNode)

  }

  private[this] def sumAtLevel[D](node: Node[D], child: BinaryTree[D], levelsMap: Map[Node[D], Int], levelsSumMap: Map[Int, D], queue: Queue[Node[D]])(implicit ev: Numeric[D]): Unit = {
    import ev.mkNumericOps
    child match {
      case childNode: Node[D] => {
        queue.enqueue(childNode)
        val level = levelsMap.get(node).get
        levelsMap.getOrElseUpdate(childNode, level + 1)
        val existingSum = levelsSumMap.getOrElse(level + 1, ev.zero)
        levelsSumMap.update(level + 1, existingSum + childNode.data)
      }
      case _ =>
    }
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


