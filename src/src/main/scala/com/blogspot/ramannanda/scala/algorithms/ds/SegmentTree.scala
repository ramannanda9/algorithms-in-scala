package com.blogspot.ramannanda.scala.algorithms.ds


/**
  * Created by Ramandeep Singh on 7/18/17.
  */
object SegmentTree {

  def left(p: Int): Int = {
    p << 1
  }

  def right(p: Int): Int = {
    (p << 1) + 1
  }

  def buildSegmentTree[K](data: Array[K], min: Boolean = true)(implicit ev: Ordering[K]): Array[Int] = {
    //larger size because we need to store upper bound of 4n nodes.
    val st = Array.ofDim[Int](data.length * 4)
    import ev.mkOrderingOps
    def buildSegmentTreeRec(p: Int, l: Int, r: Int): Unit = {
      if (l == r) {
        st(p) = l
      }
      else {
        buildSegmentTreeRec(left(p), l, (l + r) / 2)
        buildSegmentTreeRec(right(p), ((l + r) / 2) + 1, r)
        val l1 = st(left(p))
        val r1 = st(right(p))
        if (min) {
          if (data(l1) <= data(r1)) st(p) = l else st(p) = r
        }
        else {
          if (data(l1) >= data(r1)) st(p) = l else st(p) = r
        }
      }
    }
    buildSegmentTreeRec(1, 0, data.length - 1)
    st
  }
}

