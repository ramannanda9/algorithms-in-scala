package com.blogspot.ramannanda.scala.algorithms.select

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by Ramandeep Singh on 7/10/17.
  */
class QuickSelect[K, V](implicit ev: Ordering[K]) extends Selectable[K, V] {

  /**
    * It shuffles the input array
    *
    * @param data the data array
    * @return the shuffled array
    */
  private[this] def shuffle(data: Array[(K, V)]): Array[(K, V)] = {
    Random.shuffle(data.toSeq).toArray
  }

  /**
    * This method should support selecting of nth order element.
    *
    * @param data  An array of key value tuples.
    * @param index the index of the element.
    * @return the value at the index
    * @throws IndexOutOfBoundsException if the order is invalid index
    */
  @throws[IndexOutOfBoundsException]
  override def selectNthElement(data: Array[(K, V)], index: Int): V = {
    @tailrec def selectNthElementRec(data: Array[(K, V)], index: Int): V = {
      val (partIndex, partArray) = partition(data)
      if (index == partIndex) {
        partArray(index)._2
      }
      else if (index > partIndex) {
        //since 1 element is lost
        selectNthElementRec(data.slice(partIndex + 1, data.length), index - partIndex - 1)
      }
      else {
        selectNthElementRec(data.slice(0, partIndex), index)
      }
    }
    if (index < 1 || index > data.length) {
      throw new IndexOutOfBoundsException("Invalid Order element " + index)
    }
    val shuffledData = shuffle(data)
    selectNthElementRec(shuffledData, index - 1)
  }

  /**
    * It does a partition pass through the array. The gripe here is that scala does not
    * support pass by reference, so copies are being sent and returned.
    *
    * @param data the data array
    * @return the pivot index and the data array
    */
  private[this] def partition(data: Array[(K, V)]): (Int, Array[(K, V)]) = {
    import ev.mkOrderingOps
    val pivot = data(data.length - 1)._1
    var pivotIndex = data.length - 1
    var i = -1
    for (j <- 0 until data.length - 1) {
      if (data(j)._1 <= pivot) {
        i = i + 1
        val temp = data(i)
        data(i) = data(j)
        data(j) = temp
      }
    }
    pivotIndex = i + 1
    //std::swap in c++
    val temp = data(pivotIndex)
    data(pivotIndex) = data(data.length - 1)
    data(data.length - 1) = temp
    (pivotIndex, data)
  }
}

