package com.blogspot.ramannanda.scala.algorithms.select

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FlatSpec, FunSuite, Inside, Matchers}

/**
  * Created by Ramandeep Singh on 7/10/17.
  */
class QuickSelectTest extends FlatSpec with Matchers with LazyLogging{
  val dataIntKey=Array((8,"eight"),(1,"one"),(7,"seven"),(2,"two"),(5,"five"))
  val qsIntString=new QuickSelect[Int,String]()
  s"Quick Select for data ${dataIntKey.toSeq}" should "find 3rd order element to be five" in{
    qsIntString.selectNthElement(dataIntKey,3) should be ("five")
  }
  it should "find 2nd order element to be two" in{
    qsIntString.selectNthElement(dataIntKey,2) should be ("two")
  }
  it should "find 1st order element to be one" in{
    qsIntString.selectNthElement(dataIntKey,1) should be ("one")
  }
  it should "find 4th order element to be seven" in{
    qsIntString.selectNthElement(dataIntKey,4) should be ("seven")
  }
  it should "find 5th order element to be eight" in{
    qsIntString.selectNthElement(dataIntKey,5) should be ("eight")
  }
  it should "throw IndexOutOfBoundsException for 6th order element" in {
    a[IndexOutOfBoundsException] should be thrownBy{
      qsIntString.selectNthElement(dataIntKey,6)
    }
  }
  it should "throw IndexOutOfBoundsException for 0th order element" in {
    a[IndexOutOfBoundsException] should be thrownBy{
      qsIntString.selectNthElement(dataIntKey,0)
    }
  }
  val dataStringKey=Array(("b","B"),("c","C"),("a","A"),("e","E"),("d","D"))
  val qsStringString=new QuickSelect[String,String]()

  s"Quick Select for data ${dataStringKey.toSeq}" should "find 3rd order element to be C" in{
    qsStringString.selectNthElement(dataStringKey,3) should be ("C")
  }
  it should "find 2nd order element to be B" in{
    qsStringString.selectNthElement(dataStringKey,2) should be ("B")
  }
  it should "find 1st order element to be A" in{
    qsStringString.selectNthElement(dataStringKey,1) should be ("A")
  }
  it should "find 4th order element to be D" in{
    qsStringString.selectNthElement(dataStringKey,4) should be ("D")
  }
  it should "find 5th order element to be E" in{
    qsStringString.selectNthElement(dataStringKey,5) should be ("E")
  }

}
