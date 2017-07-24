package com.blogspot.ramannanda.scala.algorithms.ds


/**
  * Created by Ramandeep Singh on 7/19/17.
  * The trait is covariant because EmptyBinaryTree is BinaryTree[Nothing] and as that is covariance.
  * It is required that the trait be declared covariant on the type.
  */
trait BinaryTree[+V] {
  def left: Option[BinaryTree[V]] = this match {
    case node: Node[V] => Some(node.l)
    case _ => None
  }

  def right: Option[BinaryTree[V]] = this match {
    case node: Node[V] => Some(node.r)
    case _ => None
  }

  def size: Int = this match {
    case node: Node[V] => node.l.size + node.r.size + 1
    case EmptyBinaryTree => 0
  }

  def height: Int = {
    def getMax(bt: BinaryTree[V]): Int = bt match {
      case node: Node[V] => scala.math.max(getMax(node.l), getMax(node.r)) + 1
      case EmptyBinaryTree => 0
    }
    getMax(this) - 1
  }

}


case class Node[V](data: V, var l: BinaryTree[V], var r: BinaryTree[V], var p: Node[V]) extends BinaryTree[V] {
  override def hashCode = data.hashCode()

  override def toString = s"Data = ${data}"
}


case object EmptyBinaryTree extends BinaryTree[Nothing]

object BinaryTree {

  def mirror[V](t: BinaryTree[V]): BinaryTree[V] = t match {
    case node: Node[V] => {
      val l = node.l
      node.l = mirror(node.r)
      node.r = mirror(l)
      node
    }
    case EmptyBinaryTree => {
      EmptyBinaryTree
    }

  }

  def inOrder[V](rootNode: Node[V]): String = {
    val sb = new StringBuilder()
    def inOrderRec(t: BinaryTree[V]): Unit = {
      t match {
        case node: Node[V] => {
          inOrderRec(node.l)
          sb.append("(" + node.data.toString + ")")
          inOrderRec(node.r)
        }
        case EmptyBinaryTree =>
      }
    }
    inOrderRec(rootNode)
    sb.toString()
  }

  def preOrder[V](rootNode: Node[V]): String = {
    val sb = new StringBuilder()
    def preOrderRec(t: BinaryTree[V]): Unit = {
      t match {
        case node: Node[V] => {
          sb.append("(" + node.data.toString + ")")
          preOrderRec(node.l)
          preOrderRec(node.r)

        }
        case EmptyBinaryTree =>
      }
    }
    preOrderRec(rootNode)
    sb.toString()
  }

  def postOrder[V](rootNode: Node[V]): String = {
    val sb = new StringBuilder()
    def postOrderRec(t: BinaryTree[V]): Unit = {
      t match {
        case node: Node[V] => {
          postOrderRec(node.l)
          postOrderRec(node.r)
          sb.append("(" + node.data.toString + ")")
        }
        case EmptyBinaryTree =>
      }
    }
    postOrderRec(rootNode)
    sb.toString()
  }

}



