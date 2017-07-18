package com.blogspot.ramannanda.scala.algorithms.ds

/**
  * Created by Ramandeep Singh on 7/17/17.
  */

class UnionFind(n: Int) {
  private val rank = Array(n)
  private val p = Array(0 until n: _*)

  /**
    * Traverses the set to find the parent
    *
    * @param i the index of the element to find the representative element for
    * @return the representative element for the set.
    */
  def findSet(i: Int): Int = {
    if (p(i) == i) i else findSet(p(i))
  }

  /**
    * Checks if element indexes belong to the same set
    *
    * @param i the index of the ith element
    * @param j the index j of the jth element
    * @return true if they have the same parent else returns false.
    */
  def isSameSet(i: Int, j: Int) = {
    findSet(i) == findSet(j)
  }

  /**
    * Assigns the set with higher rank to be the parent of other set. Updates rank if both are of same rank.
    *
    * @param i ideally the index of the representative element of one set.
    * @param j ideally the index of the representative element of other set
    */
  def UnionSet(i: Int, j: Int): Unit = {
    if (!isSameSet(i, j)) {
      val x = findSet(i)
      val y = findSet(j)
      if (rank(x) > rank(y)) p(y) = x
      else {
        p(x) = y
        if (rank(x) == rank(y)) rank(y) = rank(y) + 1
      }
    }
  }
}
