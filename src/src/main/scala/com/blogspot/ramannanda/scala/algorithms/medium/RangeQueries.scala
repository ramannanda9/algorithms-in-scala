package com.blogspot.ramannanda.scala.algorithms.medium

import com.blogspot.ramannanda.scala.algorithms.ds.SegmentTree

/**
  * Created by Ramandeep Singh on 7/18/17.
  * Utilizes segment tree to answer range queries in `O(logn)` time.
  */
class RangeQueries[K](data: Array[K], min: Boolean = true)(implicit ev: Ordering[K]) {
  val st = SegmentTree.buildSegmentTree(data, min)

  import ev.mkOrderingOps

  def left(p: Int): Int = {
    p << 1
  }

  def right(p: Int): Int = {
    (p << 1) + 1
  }

  /**
    * Calculates minimum or maximum item index in a range
    *
    * @param p the parent index
    * @param l the left index
    * @param r the right index
    * @param i the startIndex
    * @param j the endIndex
    * @return the minimum or maximum item index in the range i to j
    */
  def query(p: Int, l: Int, r: Int, i: Int, j: Int): Int = {
    if (i > r || j < l) return -1
    //In range
    if (l >= i && r <= j) return st(p)
    val l1 = query(left(p), l, (l + r) / 2, i, j)
    val r1 = query(right(p), ((l + r) / 2) + 1, r, i, j)
    if (l1 == -1) return r1
    if (r1 == -1) return l1
    if (min) {
      if (data(l1) <= data(r1)) l1 else r1
    }
    else {
      if (data(l1) >= data(r1)) l1 else r1
    }
  }
}
