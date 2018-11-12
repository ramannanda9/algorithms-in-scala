package com.blogspot.ramannanda.scala.algorithms.ds.graphs

import scala.collection.mutable

object MinimumLibraryAndRoad {


  case class UnionFindWithPC(n: Int) {
    val parent = Array(0 to n: _*)
    val rank = Array.ofDim[Int](n + 1)

    def findSet(i: Int): Int = {
      if (i == parent(i)) i
      else {
        parent(i) = findSet(parent(i))
        parent(i)
      }

    }

    def isSameSet(u: Int, v: Int) = {
      findSet(u) == findSet(v)
    }

    def union(u: Int, v: Int) = {
      if (!isSameSet(u, v)) {
        val x = findSet(u)
        val y = findSet(v)
        if (rank(x) > rank(y)) parent(y) = x
        else {
          parent(x) = y
          if (rank(x) == rank(y)) rank(y) = rank(y) + 1
        }
      }
    }


  }

  // Complete the roadsAndLibraries function below.
  def roadsAndLibraries(n: Int, c_lib: Long, c_road: Long, cities: Array[Array[Int]]): Long = {
    if (c_road >= c_lib) {
      return n * c_lib
    }
    val uf = UnionFindWithPC(n)
    for (city <- cities) {
      uf.union(city(0), city(1))
    }
    val mapSets = mutable.HashMap[Int, Int]()
    for (u <- 1 to n) {
      val p = uf.findSet(u)
      if (mapSets.contains(p)) {
        mapSets.put(p, mapSets(p) + 1)
      }
      else {
        mapSets.put(p, 1)
      }
    }
    val roads = mapSets.map(_._2-1).sum
    val nodes = mapSets.keys.size
    roads * c_road * 1l + nodes * c_lib * 1l
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn


    val q = stdin.readLine.trim.toInt

    for (qItr <- 1 to q) {
      val nmC_libC_road = stdin.readLine.split(" ")

      val n = nmC_libC_road(0).trim.toInt

      val m = nmC_libC_road(1).trim.toInt

      val c_lib = nmC_libC_road(2).trim.toInt

      val c_road = nmC_libC_road(3).trim.toInt

      val cities = Array.ofDim[Int](m, 2)

      for (i <- 0 until m) {
        cities(i) = stdin.readLine.split(" ").map(_.trim.toInt)
      }

      val result = roadsAndLibraries(n, c_lib, c_road, cities)
      println(result)
    }


  }
}
