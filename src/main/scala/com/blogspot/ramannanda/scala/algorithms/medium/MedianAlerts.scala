package com.blogspot.ramannanda.scala.algorithms.medium

import java.io._

import scala.collection.mutable
import scala.util.control.Breaks._

object MedianAlerts {

  case class MovingMedian(d: Int) {
    val q = mutable.Queue[Int]()
    val arr = Array.ofDim[Int](201)

    def add(elem: Int): Unit = {
      if (q.size >= d) {
        val v = q.dequeue()
        arr(v) -= 1
      }
      q.enqueue(elem)
      arr(elem) += 1
    }

    def mMedian(): Double = {
      val a = d / 2
      val b = a + 1
      var result = 0
      var mid1 = -1
      var mid2 = -1
      breakable {
        for (i <- arr.indices) {
          result += arr(i)
          if (result >= a && mid1 == -1) {
            mid1 = i
          }
          if (result >= b) {
            mid2 = i
            break
          }
        }
      }
      if (d % 2 == 0) {
        (mid1 + mid2) / 2.0
      }
      else mid2
    }
  }

  // Complete the activityNotifications function below.
  def activityNotifications(expenditure: Array[Int], d: Int): Int = {
    var notif = 0
    val mm = MovingMedian(d)
    for (i <- 0 until d) {
      mm.add(expenditure(i))
    }
    for (i <- d until expenditure.length) {
      val median = mm.mMedian()
      if (expenditure(i) >= 2 * median) {
        notif += 1
      }
      mm.add(expenditure(i))
    }
    notif
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val nd = stdin.readLine.split(" ")

    val n = nd(0).trim.toInt

    val d = nd(1).trim.toInt

    val expenditure = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = activityNotifications(expenditure, d)
    println(result)

  }
}
