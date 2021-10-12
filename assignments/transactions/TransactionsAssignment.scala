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

  //-----------------------------------------------Q1-------------------------------------------------------
  case class Question1Result(
                              transactionDay: Int,
                              transactionTotal: Double
                            )

  def getTransactionTotal(transactions:(Int, List[Transaction])):Any = {
    //get the total transaction amount for the selected day
    val total = transactions._2.map(_.transactionAmount).sum
    Question1Result(
      transactions._1,
      total
    )
  }
  //group transactions by day
  val transactionsByDay = transactions.groupBy(_.transactionDay)
  //get the total transaction amount for each day and print the result
  val question1ResultValue = transactionsByDay.map(getTransactionTotal)
  //question1ResultValue.foreach(println)

  //-----------------------------------------------Q2-------------------------------------------------------
  case class Question2Result(
                              accountId: String,
                              categoryAvgValueMap: Map[String, Double]
                            )

  def getAverageTransactionsPerCategory(transactions: List[Transaction]): Seq[Question2Result] = {
    //groups data by ID and category
    transactions.groupBy(x => (x.accountId, x.category))
      //get all transaction amounts for each ID/category
      .mapValues(_.map(_.transactionAmount))
      //get the average for each ID/category
      .mapValues(amounts => amounts.sum/amounts.size)
      .map(x => Question2Result(
                    x._1._1,
                    Map(x._1._2 -> x._2)))
      .toSeq
      .sortBy(_.accountId)
  }
  //get the average transactions per account category and print the result
  val question2ResultValue = getAverageTransactionsPerCategory(transactions)
  //question2ResultValue.foreach(println(_))

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

  def getResults(thisTransaction:(Int,String)): Any = {
    //gets the rolling window of 5 days
    val rollingWindow = transactionsByDayAndID.filter(_._1._2 == thisTransaction._2)
      .filter(_._1._1 < thisTransaction._1)
      .filter(_._1._1 >= thisTransaction._1-5)
    //gets the transaction amounts from the rolling window
    val windowValues = rollingWindow.flatMap(_._2.map(_.transactionAmount))
    if (windowValues.nonEmpty){
      Question3Result(
        thisTransaction._1,
        thisTransaction._2,
        windowValues.max,
        windowValues.sum / windowValues.size,
        rollingWindow.flatMap(_._2.filter(_.category == "AA").map(_.transactionAmount)).sum,
        rollingWindow.flatMap(_._2.filter(_.category == "CC").map(_.transactionAmount)).sum,
        rollingWindow.flatMap(_._2.filter(_.category == "FF").map(_.transactionAmount)).sum
      )
    } else {
      Question3Result(thisTransaction._1, thisTransaction._2, 0.0, 0.0, 0.0, 0.0, 0.0) }
  }

  //uses the forID function, passing in each possible ID for the given day
  def forDays(day:Int) = {accountIDs.map(forID(_, day))}
  //pass the given day and ID into the getResults function to get statistics
  def forID(id:String, day:Int) = {getResults((day, id))}

  //group data by day and ID
  val transactionsByDayAndID = ListMap(transactions.groupBy(x => (x.transactionDay, x.accountId)).toSeq.sortBy(_._1):_*)
  //get all the distinct account ID's
  val accountIDs = transactions.map(_.accountId).distinct.sorted
  //get a list of all the days to be checked
  val days = List(6 to transactions.map(_.transactionDay).max + 1).flatten
  //get the stats for each day and ID
  val question3ResultValue = days.flatMap(forDays)
  question3ResultValue.foreach(println)
}