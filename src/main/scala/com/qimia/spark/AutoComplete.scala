package com.qimia.spark

case class AutoComplete(terms:Seq[Term]) {

  def numberOfMatches(prefix:String):Integer = {
    var result = 0

    val t:Term = Term(prefix,0)
    val start: Int = Term.firstIndexOf(terms,t,prefix.length)
    val end:Int = Term.lastIndexOf(terms,t,prefix.length)
    if(start >= 0)
      result = end-start +1

    result


  }

  def allMatches(prefix:String):List[Term] = {

    val t:Term = Term(prefix,0)
    var start: Int = Term.firstIndexOf(terms,t,prefix.length)
    val size = numberOfMatches(prefix)

    var matches = List[Term]()

    for( i <- 0 until size) {
      matches ::=  terms(start)
      start = start + 1
    }

    matches.sortWith(_.weight > _.weight)




  }
}
