package com.blogspot.ramannanda.scala.algorithms.medium

import scala.collection.mutable

object PartitionLabels {
  def partitionLabels(S: String): List[Int] = {
    val hm = mutable.HashMap[Char, (Int, Int)]()
    for (i <- S.toCharArray.indices) {
      if (hm.contains(S(i))) {
        val ex = hm(S(i))
        hm.put(S(i), (ex._1, i))
      }
      else {
        hm.put(S(i), (i, i))
      }
    }
    val si = hm.values.toVector.sortBy(_._1)
    var currMax = si(0)._2
    var start = 0
    var result = Vector[Int]()
    for (i <- 1 until si.length) {
      if (si(i)._1 > currMax) {
        result = result :+ (currMax - start + 1)
        start = si(i)._1
        currMax = si(i)._2
      } else {
        currMax = Math.max(currMax, si(i)._2)
      }
    }
    result = result :+ (currMax - start + 1)
    result.toList
  }

  def main(args: Array[String]): Unit = {
    partitionLabels("ababcbacadefegdehijhklij")
  }
}
