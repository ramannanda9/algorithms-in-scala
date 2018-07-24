package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.rl

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.control.Breaks._

//uva 00161
object TrafficLights {

  def solveFirstGreenAfterOrange(cycles: Seq[Int]): Int = {

    val time = Array.ofDim[Int](cycles.size)
    var s = cycles.reduce(Math.min)
    while (s <= 18000) {
      var allOverlap = true
      breakable {
        for (i <- time.indices) {
          /*
        initially this will lead to multiply by 2 and later on only those values
        are multiplied till there time+cycle-5<=s
         */
          if (time(i) + cycles(i) - 5 <= s) {
            time(i) += cycles(i) << 1
          }
          if (!(time(i) <= s && s < time(i) + cycles(i) - 5)) {
            s = time(i) - 1
            allOverlap = false
            break
          }
        }
      }
      if (allOverlap) return s
      s += 1
    }
    s
  }

  def main(args: Array[String]): Unit = {
    var lb = ListBuffer[Int]()
    while (true) {
      val inputLine = StdIn.readLine().trim
      if (inputLine.equals("0 0 0")) return
      val cycles = inputLine.split("\\s+").map(_.toInt)
      if (cycles(cycles.length - 1) == 0) {
        lb ++= cycles.slice(0, cycles.length - 1)
        val result = solveFirstGreenAfterOrange(lb)
        if (result > 18000) {
          println(s"Signals fail to synchronise in 5 hours")
        }
        else {
          val hours = result / (60 * 60)
          val mins = (result % 3600) / 60
          val seconds = result % 60
          println(s"$hours:$mins:$seconds")
        }
        lb = new ListBuffer[Int]
      }
      else {
        lb ++= cycles

      }
    }
  }
}
