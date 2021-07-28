package com.qimia.spark

import Utilities._

import scala.io.Source
import scala.io.StdIn.readLine




object Main {

  case class Term(query:String,weight:Long)

  case class AutoComplete(terms:List[Term]) {
    def numberOfMatches(prefix:String):Integer = {

      var result = 0

      val t:Term = Term(prefix,0)
      val start: Int = firstIndexOf(terms,t,prefix.length)
      val end:Int = lastIndexOf(terms,t,prefix.length)
      if(start >= 0)
        result = end-start +1

      result


    }

    def allMatches(prefix:String):List[Term] = {

      val t:Term = Term(prefix,0)
      var start: Int = firstIndexOf(terms,t,prefix.length)
      var size = numberOfMatches(prefix)

      var matches = List[Term]()

      for( i <- 0 until size) {
        matches ::=  terms(start)
        start = start + 1
      }

      matches.sortWith(_.weight > _.weight)




    }
  }

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

      val sortedTerms = terms.sortWith(_.query < _.query)

      val autoComplete :AutoComplete = AutoComplete(sortedTerms)

      println(s"********** ${autoComplete.numberOfMatches(key.toLowerCase)} matches **********")
      autoComplete.allMatches(key.toLowerCase).foreach(term => println(s"Term: ${term.query} Weight: ${term.weight}"))
    }

  }
}
