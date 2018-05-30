package com.blogspot.ramannanda.scala.algorithms.cp3.medium

/**
  * Solution to uva 00573
  */
object SnailClimb {

  def dayOfSuccessOrFailure(h: Double, u: Double, d: Double, f: Double): Unit = {
    var currentDay = 0
    var currentHeight = 0.0
    var climbHeight = u
    val fatigue = u * f
    while (true) {
      currentHeight = currentHeight + climbHeight
      currentDay = currentDay + 1
      //first check if height exceeds
      if (currentHeight > h) {
        println(s"success on $currentDay")
        return
      }
      currentHeight = currentHeight - d
      //then check if either current height falls below 0 or climb height is less than 0
      if (currentHeight < 0.0 || climbHeight < 0.0) {
        println(s"failure on $currentDay")
        return
      }
      //At end as the fatigue is applicable next day
      climbHeight = climbHeight - fatigue
    }
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      val line = scala.io.StdIn.readLine().split("\\s+")
      val height = line(0).trim.toDouble
      if (height <= 0 + 0.001) {
        return
      }
      val u = line(1).trim.toDouble
      val d = line(2).trim.toDouble
      val f = line(3).trim.toDouble / 100
      dayOfSuccessOrFailure(height, u, d, f)
    }
  }
}
