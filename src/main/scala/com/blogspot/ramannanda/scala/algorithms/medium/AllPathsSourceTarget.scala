package com.blogspot.ramannanda.scala.algorithms.medium

import scala.collection.mutable.ListBuffer

object AllPathsSourceTarget {
  def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {
    traverse(0, graph)
  }

  def traverse(u: Int, graph: Array[Array[Int]]): List[List[Int]] = {
    var lb = ListBuffer[List[Int]]()

    def traverseRec(u: Int, lst: List[Int]): Unit = {
      if (u == graph.length - 1) {
        lb += lst :+ u
      }
      for (v <- graph(u)) {
        traverseRec(v, lst :+ u)
      }
    }

    traverseRec(0, List())
    lb.toList
  }

  def main(args: Array[String]): Unit = {
    println(allPathsSourceTarget(Array(Array(4, 3, 1), Array(3, 2, 4), Array(3), Array(4), Array())))
  }
}
