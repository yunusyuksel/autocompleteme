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

  def allMatches(prefix:String):Seq[Term] = {

    val t:Term = Term(prefix,0)
    val start: Int = Term.firstIndexOf(terms,t,prefix.length)
    val size = numberOfMatches(prefix)

    val slicedTerms = terms.slice(start,start+size)

    slicedTerms.sortWith(_.weight > _.weight)



  }
}
