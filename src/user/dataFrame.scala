package user
import scala.language.implicitConversions
import util.config.Dict
import util.config.{ NSInt, NSDouble, NSString, NSLong, NumScalaType }
import util.config._
import util.baseCalc
import math.pow
import java.lang.invoke.LambdaForm
object dataFrame {
  class LiteralSeries(source: Vector[Any], index: Boolean = false) {
    var name: String = ""
    val data: Vector[String] = if (source.length == 0) { name = "empty"; Vector[String]() }
    else {
      name = source.head.toString
      source.tail.map(_.toString)
    }
    def unique():Set[String]={
      data.toSet
    }
    def count(f:String=>Boolean):Int={
      data.count(f)
    }
    def frequency[K](f:String=>K):Map[K,Double]={
       data.groupBy(f).mapValues((x:Vector[String])=> 1.0*x.length/data.length)
    }
    //I feel sooooooooooooooooooooo charming!
  }
  class NumberSeries[T <% Double: Numeric](Type: NumScalaType[T])(source: Vector[Any], index: Boolean = false) {
    var name: String = ""
    val data: Vector[T] = if (source.length == 0) { name = "empty"; Vector[T]() }
    else
      (index match {
        case true =>
          name = source.head.toString; source.tail
        case false => name = "unname"; source
      }) then {
        Type match {
          case NSInt    => _.map(_.toString then baseCalc.Str2Int).asInstanceOf[Vector[T]]
          case NSDouble => _.map(_.toString then baseCalc.Str2Double).asInstanceOf[Vector[T]]
          case NSString => _.map(_.toString).asInstanceOf[Vector[T]]
          case NSLong   => _.map(_.toString then baseCalc.Str2Long).asInstanceOf[Vector[T]]
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
 
  

