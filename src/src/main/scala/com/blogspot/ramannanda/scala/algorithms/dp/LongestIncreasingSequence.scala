package com.blogspot.ramannanda.scala.algorithms.dp

/**
  * Created by Ramandeep Singh on 7/17/17.
  */
class LongestIncreasingSubSequenceDP[K](implicit ev: Ordering[K]) {
  /**
    * Look at the corresponding LISS.md accompanying problem
    * Complexity `O(N^2)`
    * There is a much faster implementation via Kadane's algorithm
    *
    * @param data the array in which to find
    * @return the indexes of elements
    */
  def getLICS(data: Array[K]): Vector[K] = {
    import ev.mkOrderingOps
    var result = Vector[K]()
    val p = Array.fill(data.length)(0)
    val liss = Array.fill(data.length)(1)
    for (i <- data.indices) {
      var index = 0
      for (j <- 0 until i) {
        if (data(j) < data(i)) {
          liss(i) = liss(j) + 1
          index = j
        }
        p(i) = index
      }
    }
    var max = liss(0)
    var maxIndex = 0
    for (i <- 1 until liss.length) {
      if (max < liss(i)) {
        max = liss(i)
        maxIndex = i
      }
    }
    //backtrack from max index
    var i = maxIndex
    //add it
    result :+= data(maxIndex)
    while (i > 0) {
      result :+= data(p(i))
      i = p(i)
    }
    result.reverse
  }
}
