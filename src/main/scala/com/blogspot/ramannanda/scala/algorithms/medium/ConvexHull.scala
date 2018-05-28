package com.blogspot.ramannanda.scala.algorithms.medium

object ConvexHull {
  type Point = (Int, Int)

  /**
    * Gives the direction of movement
    *
    * Slope of two lines is compared
    *
    * @param p the initial point
    * @param q the point in the middle
    * @param r the last point
    * @return 1 if clockwise 0 if collinear and -1 if counter clockwise
    */
  def getDirection(p: Point, q: Point, r: Point): Int = {
    val direction = ((q._2 - p._2) * (r._1 - q._1)) - ((q._1 - p._1) * (r._2 - q._2))
    direction match {
      case t if t < 0 => -1
      case t if t > 0 => 1
      case t if t == 0 => 0
    }
  }

  def getPerimeter(points: Vector[Point]): Double = {
    val numPoints = points.size
    //get the leftMost point
    var leftMost = 0
    for (i <- 1 until points.length) {
      if (points(i)._2 < points(leftMost)._2 || (points(i)._1 == points(leftMost)._1 && points(i)._2 < points(leftMost)._2)) {
        leftMost = i
      }
    }
    var p = leftMost
    var pointIndexes = Vector[Int]()
    do {
      pointIndexes = pointIndexes :+ p
      var q = (p + 1) % numPoints
      for (i <- points.indices) {
        if (getDirection(points(p), points(i), points(q)) == -1) {
          q = i
        }
      }
      p = q
    } while (p != leftMost)
    var perimeter = 0.0
    var prevPoint = points(pointIndexes(0))
    for (i <- 1 until pointIndexes.size) {
      val currentPoint = points(pointIndexes(i))
      perimeter = perimeter + Math.sqrt(Math.pow(currentPoint._2 - prevPoint._2, 2) + Math.pow(currentPoint._1 - prevPoint._1, 2))
      prevPoint = currentPoint
    }
    perimeter=perimeter+Math.sqrt(Math.pow(points(pointIndexes(0))._2 - prevPoint._2, 2) + Math.pow(points(pointIndexes(0))._1 - prevPoint._1, 2))
    perimeter
  }

  def main(args: Array[String]) {
    val numPoints = scala.io.StdIn.readLine().trim.toInt
    println(numPoints)
    var points = Vector[Point]()
    for (i <- 0 until numPoints) {
      val xy = scala.io.StdIn.readLine().split("\\s+")
      points = points :+ (xy(0).toInt, xy(1).toInt)
    }
    getPerimeter(points)


  }
}
