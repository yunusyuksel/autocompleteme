package com.qimia.spark

case class Term(query:String,weight:Long) extends Ordered[Term]{
  if("".equals(query) || weight < 0)
      throw new IllegalArgumentException("query must not be null or weight must not be negatif")

  override def toString = s"Term: $query ---- Weight: $weight"

  def compare(that:Term) = {
    this.query.compare(that.query)
  }
}

object Term {
  def firstIndexOf(terms:Seq[Term],key:Term,len:Int):Int = {
    var result:Int= -1
    var high:Int = terms.length

    var low = 0
    var mid:Int = 0

    import scala.util.control.Breaks._

    breakable{
      while(high >= low){
        mid = low + (high - low)/2;

        val comp:Int = compare(key,terms(mid),len)

        if(comp < 0)
          high = mid-1
        else if(comp > 0)
          low = mid + 1
        else if (mid == low) {
          result = mid
          break
        } else
          high=mid

      }
    }

    result
  }


  def lastIndexOf(terms:Seq[Term],key:Term,len:Int):Int = {

    var result:Int= -1
    var high:Int = terms.length -1

    var low = 0
    var mid:Int = 0

    import scala.util.control.Breaks._

    breakable{
      while(high >= low){
        mid = low + (high - low + 1 ) /2;

        val comp:Int = compare(key,terms(mid),len)

        if(comp < 0)
          high = mid-1
        else if(comp > 0)
          low = mid + 1
        else if (mid == high) {
          result = mid
          break
        } else
          low=mid

      }
    }

    result

  }

  def compare(o1:Term,o2:Term,prefix:Int) = {

    var result = 0

    import scala.util.control.Breaks._
    breakable{
      for{
        i <- 0 to prefix
        if i<prefix
        if i < o1.query.length
        if i < o2.query.length

      }{
        if(o1.query.charAt(i) < o2.query.charAt(i)) {
          result = -1
          break
        } else if(o1.query.charAt(i) > o2.query.charAt(i)) {
          result = 1
          break
        }
      }
    }

    result


  }
}
