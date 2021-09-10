/*
  Quantexa Copyright Statement
 */

package com.quantexa.assignments.transactions

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.Try

/***
  * This Object holds the functions required for the Quantexa coding exercise and an entry point to execute them.
  * Once executed each question is executed in turn printing results to console
  */
  
object TransactionAssignment extends App {

  /***
    * A case class to represent a transaction
    * @param transactionId The transaction Identification String
    * @param accountId The account Identification String
    * @param transactionDay The day of the transaction
    * @param category The category of the transaction
    * @param transactionAmount The transaction value
    */
  case class Transaction(
                          transactionId: String,
                          accountId: String,
                          transactionDay: Int,
                          category: String,
                          transactionAmount: Double)

  //The full path to the file to import
  val fileName = getClass.getResource("/transactions.csv").getPath

  //The lines of the CSV file (dropping the first to remove the header)
  //  Source.fromInputStream(getClass.getResourceAsStream("/transactions.csv")).getLines().drop(1)
  val transactionLines: Iterator[String] = Source.fromFile(fileName).getLines().drop(1)

  //Here we split each line up by commas and construct Transactions
  val transactions: List[Transaction] = transactionLines.map { line =>
    val split = line.split(',')
    Transaction(split(0), split(1), split(2).toInt, split(3), split(4).toDouble)
  }.toList

  /*
   * 
   * END PROVIDED CODE
   * 
   */

  //-----------------------------------------------Q3-------------------------------------------------------
  case class Question3Result(
                            transcationDay:Int,
                            accountId:String,
                            max:Double,
                            avg:Double,
                            aaTotal:Double,
                            ccTotal:Double,
                            ffTotal:Double
                            )

  def getMinDay(day:Int):Int = {
    if (day >= 5) day-5
    else 1
  }

  def getResults(thisTransaction:((Int,String), List[Transaction])): Any = {
    val rollingWindow = transactionsByDayAndID.filter(_._1._2 == thisTransaction._1._2)
      .filter(_._1._1 < thisTransaction._1._1)
      .filter(_._1._1 > getMinDay(thisTransaction._1._1))
    val windowValues = rollingWindow.flatMap(_._2.map(_.transactionAmount))
    if (windowValues.nonEmpty){
      Question3Result(
        thisTransaction._1._1,
        thisTransaction._1._2,
        windowValues.max,
        windowValues.sum / windowValues.size,
        rollingWindow.flatMap(_._2.filter(_.category == "AA").map(_.transactionAmount)).sum,
        rollingWindow.flatMap(_._2.filter(_.category == "CC").map(_.transactionAmount)).sum,
        rollingWindow.flatMap(_._2.filter(_.category == "FF").map(_.transactionAmount)).sum
      )
    } else { Question3Result(thisTransaction._1._1, thisTransaction._1._2, 0.0, 0.0, 0.0, 0.0, 0.0) }
  }
  val transactionsByDayAndID = ListMap(transactions.groupBy(x => (x.transactionDay, x.accountId)).toSeq.sortBy(_._1):_*)
  val question3ResultValue = transactionsByDayAndID.map(getResults)
  question3ResultValue.foreach(println)

}