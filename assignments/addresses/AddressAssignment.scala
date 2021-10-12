package com.quantexa.assignments.addresses

import scala.:+
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

/***
  *  You have been given a dataset containing a list of addresses, along with customers who lived at the addresses
  *  and the start and end date that they lived there. Here is an example:
  *
  *     Customer ID 	Address ID 	From date 	To_date
  *     IND0003	      ADR001	    727	        803
  *     IND0004	      ADR003	    651	        820
  *     IND0007	      ADR003	    1710	      1825
  *     IND0008	      ADR005	    29	        191
  *     IND0001	      ADR003	    1777	      1825
  *     IND0002	      ADR003	    1144        1158
  *
  *  Write an algorithm for the following:
  *
  *  "For each address, calculate all of the groups of customers who have lived at the address at overlapping times."
  *
  *  Note that each customer in the group only needs to overlap with at least one other customer in the group, so there
  *  may be pairs of customers in the group who never lived at the address at the same time.
  *
  *  The algorithm should output the following columns:
  *  •	 The address
  *  •	 The list of customers in the group
  *  •	 The first date that any customer in the group lived at the address
  *  •	 The last date that any customer in the group lived at the address
  *
  *  Example single row of output:
  *
  *  Address_ID 	Group_Customers	    Group_Start	  Group_End
  *  ADR003	      [IND0001,IND0007]	  1710	        1825
  *
  */
  
  
object AddressAssignment extends App{

  //Define a case class AddressData which stores the occupancy data
  case class AddressData(
                          customerId: String,
                          addressId: String,
                          fromDate: Int,
                          toDate: Int
                        )

  //The full path to the file to import
  val fileName = getClass.getResource("/address_data.csv").getPath

  //The lines of the CSV file (dropping the first to remove the header)
  //  Source.fromInputStream(getClass.getResourceAsStream("/address_data.csv")).getLines().drop(1)
  val addressLines = Source.fromFile(fileName).getLines().drop(1)

  val occupancyData: List[AddressData] = addressLines.map {
    line =>
      val split = line.split(',')
      AddressData(split(0), split(1), split(2).toInt, split(3).toInt)
  }.toList

  //END GIVEN CODE

  case class AddressGroupedData(
                                 group:Long,
                                 addressId:String,
                                 customerIds:Seq[String],
                                 startDate:Int,
                                 endDate:Int
                               )

  //sort data by ID and fromDate
  val dataByIdAndDate = occupancyData.sortBy(x => (x.addressId, x.fromDate))
  //create empty initial group
  val firstGroup = AddressGroupedData(0, "", List[String](), 0, 0)
  //get groups of customers based on address and stay date
  val addressGroups = groupedDataByStay(dataByIdAndDate, firstGroup, List[AddressGroupedData]())
  //get number of address groups
  val numberOfGroups = addressGroups.length

  @tailrec
  def groupedDataByStay(addressList: List[AddressData], currentGroup: AddressGroupedData, groupedDataList:
  List[AddressGroupedData]):List[AddressGroupedData] = {
    addressList match {
      case Nil =>
        currentGroup :: groupedDataList
      case x :: xs =>
      if(currentGroup.addressId != x.addressId || x.fromDate > currentGroup.endDate){
        //if there is a current group add it to groupedDataList, else return the groupedDataList
        val newGroupedData = if(currentGroup.addressId != "") currentGroup :: groupedDataList else groupedDataList
        //create a new current group
        val newCurrentGroup = AddressGroupedData(
          currentGroup.group + 1,
          x.addressId,
          List(x.customerId),
          x.fromDate,
          x.toDate
        )
        //call the recursive function to check if more addresses need to be added to the group
        groupedDataByStay(xs, newCurrentGroup, newGroupedData)
      }
    else {
        //create a new current group, update the end date of the group and add the customer ID to the sequence
        val newCurrentGroup = currentGroup.copy(endDate = math.max(x.toDate, currentGroup.endDate), customerIds =
        currentGroup.customerIds :+ x.customerId)
        //call the recursive function to check if more addresses need to be added to the group
        groupedDataByStay(xs, newCurrentGroup, groupedDataList)
      }
    }
  }
  //print each group and the total number of groups
  addressGroups.foreach(println(_))
  println(numberOfGroups)
}

