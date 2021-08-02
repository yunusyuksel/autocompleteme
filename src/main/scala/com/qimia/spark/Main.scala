package com.qimia.spark

import scala.io.Source
import scala.io.StdIn.readLine



object Main {


  def main(args:Array[String]) = {

    println("Choose the file \n 1-wiktionary.txt \n 2-cities.txt ")
    val fileInput = readLine()

    val filePrefix = "data"

    val filename =  fileInput match{
      case "1" => filePrefix + "/wiktionary.txt"
      case "2" => filePrefix + "/cities.txt"
    }

    var terms = List[Term]()

    for(line <- Source.fromFile(filename).getLines){
      val lineArr = line.trim.split("\\s")
      val weight = lineArr(0).toLong
      val term = lineArr(1).toLowerCase()
      terms ::= Term(term,weight)

    }

    while(true){

      print("Enter the key to search for: ")
      val key = readLine()

      val sortedTerms = terms.sorted.toSeq


      val autoComplete :AutoComplete = AutoComplete(sortedTerms)

      println(s"********** ${autoComplete.numberOfMatches(key.toLowerCase)} matches **********")
      autoComplete.allMatches(key.toLowerCase).foreach(println)
    }

  }
}
