package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.maps

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn

object CountMaps {
  def freqQuery(queries: Array[Array[Int]]): Array[Int] = {
    val opMap = mutable.HashMap[Int, Int]()
    val countMap = mutable.HashMap[Int, Int]()
    val buffOps = ListBuffer[Int]()
    for (arr <- queries) {
      arr(0) match {
        case 1 =>
          var count = 1
          if (opMap.get(arr(1)).isDefined) {
            count = opMap(arr(1)) + 1
            opMap.put(arr(1), count)
          } else {
            opMap.put(arr(1), 1)
          }
          if (countMap.get(count).isDefined) {
            countMap.put(count, countMap(count) + 1)
          }
          else {
            countMap.put(count, 1)
          }
          if (countMap.get(count - 1).isDefined) {
            countMap.put(count - 1, countMap(count - 1) - 1)
          }
        case 2 =>
          if (opMap.get(arr(1)).isDefined) {
            val ec = opMap(arr(1))
            if (ec - 1 == 0) {
              opMap.remove(arr(1))
            }
            opMap.put(arr(1), ec - 1)
            if (countMap.get(ec).isDefined) {
              countMap.put(ec, countMap(ec) - 1)
            }
            if (countMap.get(ec - 1).isDefined) {
              countMap.put(ec - 1, countMap(ec - 1) + 1)
            }
          }
        case 3 =>
          var toAppend = 0
          if (countMap.get(arr(1)).isDefined) {
            if (countMap(arr(1)) > 0) {
              toAppend = 1
            }
          }
          buffOps += toAppend
      }
    }
    buffOps.toArray

  }

  def main(args: Array[String]) {


    val q = StdIn.readLine.trim.toInt

    val queries = Array.ofDim[Int](q, 2)

    for (i <- 0 until q) {
      queries(i) = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
    }

    val ans = freqQuery(queries)
    println(ans.mkString("\n"))

  }

}
