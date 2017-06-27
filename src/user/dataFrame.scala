package user
import scala.language.implicitConversions
import scala.reflect.{ClassTag,classTag}
import util.config._
import util.baseCalc
import math.pow



object dataFrame {
  abstract class Series[T]{
     var name:String
     val data:Vector[T]
  }
  
  trait countable[T] extends Series[T]{
    def count (f:T=>Boolean):Int=data.count(f)
    def frequency [K](f:T=>K):Map[K,Double]=data.groupBy(f).mapValues((x:Vector[T])=> 1.0*x.length/data.length)
    def unique ():Set[T]=data.toSet
  }
  
  class LiteralSeries(source: Vector[Any], index: Boolean = false) extends Series[String] with countable[String] {
    var name: String = ""
    val data: Vector[String] =
      if (source.length == 0) { name = "empty"; Vector[String]() }
      else {
        (index match {
          case true => name = source.head.toString ; source.tail
          case false => name = "unname"; source
         }) then (_.map(_.toString))
      }
    //I feel sooooooooooooooooooooo charming!
  }
  class NumericSeries[T <% Double: Numeric:ClassTag](source: Vector[Any], index: Boolean = false) extends countable[T]{
    var name: String = ""
    val data: Vector[T] = if (source.length == 0) { name = "empty"; Vector[T]() }
    else
      (index match {
        case true =>
          name = source.head.toString; source.tail
        case false => name = "unname"; source
      }) then {
        classTag[T] match {
          case ClassTag.Int    => _.map(_.toString then baseCalc.Str2Int).asInstanceOf[Vector[T]]
          case ClassTag.Double => _.map(_.toString then baseCalc.Str2Double).asInstanceOf[Vector[T]]
          case ClassTag.Long   => _.map(_.toString then baseCalc.Str2Long).asInstanceOf[Vector[T]]
        }
      }
    var nanNum = data.count((x: T) => x == null)
    var tolNum = data.length
    var notNanNum = tolNum - nanNum

    def sum(isPar: Boolean = false): T = baseCalc.sum(data, isPar)
    def nansum(isPar: Boolean = false): T = baseCalc.nansum(data, isPar)

    def mean(isPar: Boolean = false): Double = sum(isPar) / tolNum
    def nanmean(isPar: Boolean = false): Double = nansum(isPar) / tolNum

    //for n-order center moment . variance is the  "2-order center moment"
    def ocm(order: Int, isPar: Boolean = false): Double = {
      val meanVal: Double = mean(isPar)
      val f = (x: T) => x - meanVal
      baseCalc.sumWith(data, f, isPar)
    }
    def nanocm(order: Int, isPar: Boolean = false): Double = {
      val meanVal: Double = mean(isPar)
      val f = (x: T) => x - meanVal
      baseCalc.nansumWith(data, f, isPar)
    }

  }

}
 
  

