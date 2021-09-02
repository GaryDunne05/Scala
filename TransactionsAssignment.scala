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
  def getTransactionTotal(transactions:Map[Int, List[Transaction]]):List[Int] ={for(i <- 1 to transactions.keys.size){
      var sum = 0.0
      for(x <- 0 to transactions(i).length-1){
        sum += transactions(i)(x).transactionAmount
      }
      println("Day " + transactions(i)(0).transactionDay + ": " + sum)}
    List()
  }
  val transactionsByDay = transactions.groupBy(_.transactionDay)
  val myMap = ListMap(transactionsByDay.toSeq.sortBy(_._1):_*)
  //println(myMap.values.map(_.map(_.transactionAmount).sum))
  //getTransactionTotal(transactionsByDay)

  //-----------------------------------------------Q2-------------------------------------------------------
  case class Question2Result(
                            accountId: String,
                            categoryAvgValueMap: Map[String, Double]
                            )

  def getAverageTransactionsPerAccountType(transactions: List[Transaction]): Seq[Question2Result] = {
    transactions.groupBy(x => (x.accountId, x.category))
      .mapValues(_.map(_.transactionAmount))
      .mapValues(amounts => amounts.sum / amounts.size)
      .map(x => Question2Result(x._1._1, Map(x._1._2 -> x._2)))
      .toSeq
      .sortBy(_.accountId)
  }
  val question2ResultValue = getAverageTransactionsPerAccountType(transactions)
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
  val loop = List.range(1, transactions.groupBy(_.transactionDay).toSeq.length + 1)
  var rolling = Map()
  val daySizes = transactionsByDay.map(_._2.length)

  for (i <- loop){ //looping through transactionsByDay
    print(i + ": ")
    var newWindow = Double
    for (j <- i-5 to i-1){ //looping through rolling window
      if (j > 0){
        print(j + " ")
      }
    }
    println()
  }
  val transactionsByAccount = transactions.groupBy(_.accountId)
  var max = 0.0
  for (i <- myMap.keys){
    val currentList = myMap.get(i)
  }
  val transactionsByDayAndID = myMap.mapValues(x => x.sortBy(_.accountId))
  for (i <- transactionsByDayAndID.keys){
    transactionsByDayAndID.get(i).foreach(println(_))
    }
}