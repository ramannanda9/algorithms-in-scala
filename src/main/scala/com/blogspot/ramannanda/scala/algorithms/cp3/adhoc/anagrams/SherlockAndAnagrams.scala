package com.blogspot.ramannanda.scala.algorithms.cp3.adhoc.anagrams

import scala.collection.mutable

object SherlockAndAnagrams {
  def sherlockAndAnagrams(s: String): Int = {
    val dict=mutable.HashMap[String,Int]()
    for(i <- 0 until s.length){
      for(j <- (i+1) to s.length){
        val chars=s.substring(i,j)
        val sorted=chars.sorted
        if(dict.contains(sorted)){
          dict.put(sorted,dict(sorted)+1)
        }
        else{
          dict.put(sorted,1)
        }
      }
    }
    dict.values.map(n=>(n*(n-1))/2).sum
  }

  def main(args: Array[String]): Unit = {
    print(sherlockAndAnagrams("abba"))
  }

}
