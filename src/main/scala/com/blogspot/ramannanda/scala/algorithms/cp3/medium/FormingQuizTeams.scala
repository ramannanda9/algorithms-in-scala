package com.blogspot.ramannanda.scala.algorithms.cp3.medium

import scala.util.control.Breaks._

/**
  * UVA 10911 Problem is to minimize the overall cost of distance between pair of points
  */
object FormingQuizTeams {
  type Point = (Int, Int)

  def doMatching(bitmask: Int, n: Int, distanceTable: Array[Array[Double]]): Double = {

    val dp = Array.fill(1 << 2 * n)(-1.0)
    val target = (1 << 2 * n) - 1

    /**
      * Computes the minimum distance for bitmask until all are matched
      *
      * @param bitmask the bitmask
      * @return the minimum distance
      */
    def doMatchingRec(bitmask: Int): Double = {
      if (dp(bitmask) > -0.5) {
        return dp(bitmask)
      }
      if (bitmask == target) {
        dp(bitmask) = 0
        return dp(bitmask)
      }
      var p1 = 0
      //Some high value
      var cost = 50000000.0
      breakable {
        for (i <- 0 until 2 * n) {
          p1 = i
          if ((bitmask & (1 << p1)) == 0) break
        }
      }
      for (p2 <- p1 + 1 until 2 * n) {
        if ((bitmask & (1 << p2)) == 0) {
          cost = Math.min(cost, distanceTable(p1)(p2) + doMatchingRec(bitmask | 1 << p1 | 1 << p2))
        }
      }
      dp(bitmask) = cost
      cost
    }

    doMatchingRec(bitmask)
  }

  def main(args: Array[String]): Unit = {
    val numPoints = scala.io.StdIn.readLine().trim.toInt
    var points = Vector[Point]()
    for (i <- 0 until numPoints) {
      val xy = scala.io.StdIn.readLine().split("\\s+")
      points = points :+ (xy(0).toInt, xy(1).toInt)
    }
    val distanceTable = Array.ofDim[Double](numPoints, numPoints)
    for (i <- 0 until numPoints - 1) {
      for (j <- i + 1 until numPoints) {
        val distance = Math.hypot(points(i)._1 - points(j)._1, points(i)._2 - points(j)._2)
        distanceTable(i)(j) = distance
        distanceTable(j)(i) = distance
      }
    }
    println(s"distance is  ${doMatching(0, points.size / 2, distanceTable)}")
  }
}
