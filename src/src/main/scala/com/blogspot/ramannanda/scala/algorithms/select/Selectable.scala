package com.blogspot.ramannanda.scala.algorithms.select

/**
  * Created by Ramandeep Singh on 7/10/17.
  */
trait Selectable[K, V] {
  /**
    * This method should support selecting of nth order element.
    * @param data An array of key value tuples.
    * @param index the index of the element.
    * @return the value at the index
    * @throws IndexOutOfBoundsException if the order is invalid index
    */
  @throws[IndexOutOfBoundsException]
  def selectNthElement(data: Array[(K, V)], index: Int): V

}
