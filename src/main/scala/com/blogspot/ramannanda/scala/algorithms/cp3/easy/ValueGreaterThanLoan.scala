package com.blogspot.ramannanda.scala.algorithms.cp3.easy

/**
  * Solution to UVA p10114
  */
object ValueGreaterThanLoan {
  type DepRecord = (Int, Double)

  def valueExceedsOwnedAmount(carValue: Double, downPayment: Double, loanMonths: Int,
                              depRecords: Seq[DepRecord]): Int = {
    var crValue = carValue
    var amtOwed = carValue - downPayment
    val monthlyInstallment = amtOwed / loanMonths
    var depRates = Vector[Double]()
    var currentMonth = 0
    for (i <- 0 until depRecords.size - 1) {
      for (j <- depRecords(i)._1 until depRecords(i + 1)._1) {
        depRates = depRates :+ depRecords(i)._2
        currentMonth = j
      }
    }
    for (i <- currentMonth until loanMonths - 1) {
      depRates = depRates :+ depRecords.last._2
    }
    currentMonth = 0
    for (depRate <- depRates) {
      crValue = crValue * (1 - depRate)
      if (currentMonth != 0) {
        amtOwed = amtOwed - monthlyInstallment
      }
      if (crValue > amtOwed) return currentMonth
      currentMonth = currentMonth + 1
    }
    currentMonth
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      val line = scala.io.StdIn.readLine().split("\\s+")
      val duration = line(0).trim.toInt
      if (duration < 0) return
      val downPayment = line(1).trim.toDouble
      val loanAmount = line(2).trim.toDouble
      val numDepRecords = line(3).trim.toInt
      val carValue = downPayment + loanAmount
      var depRecords = Vector[DepRecord]()
      for (i <- 0 until numDepRecords) {
        val depRecord = scala.io.StdIn.readLine().split("\\s+")
        val depMonth = depRecord(0).trim.toInt
        val depPercent = depRecord(1).trim.toDouble
        depRecords = depRecords :+ (depMonth, depPercent)
      }
      val months = valueExceedsOwnedAmount(carValue, downPayment, duration, depRecords)
      if (months > 1) {
        println(s"$months months")
      }
      else {
        println(s"$months month")
      }

    }
  }
}
