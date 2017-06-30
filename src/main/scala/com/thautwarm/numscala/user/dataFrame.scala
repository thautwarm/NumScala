package com.thautwarm.numscala.user

import com.thautwarm.numscala.util.baseCalc
import com.thautwarm.numscala.util.config._

import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

object dataFrame {

  abstract class Series[T] {
    var name: String
    val data: Vector[T]

  }

  trait hasNaN[T] extends Series[T] {
    def nanNum() = data.count((x: T) => x == null)

    def tolNum() = data.length

    def notNanNum() = tolNum - nanNum
  }

  trait countable[T] extends Series[T] {
    def count(f: T => Boolean): Int = data.count(f)

    def frequency[K](f: T => K): Map[K, Double] = data.groupBy(f).mapValues((x: Vector[T]) => 1.0 * x.length / data.length)

    def unique(): Set[T] = data.toSet
  }

  abstract class Numerical[T <% Double : Numeric : ClassTag] extends Series[T] with hasNaN[T] {
    def sum(isPar: Boolean = false): T = baseCalc.sum(data, isPar)

    def nansum(isPar: Boolean = false): T = baseCalc.nansum(data, isPar)

    def mean(isPar: Boolean = false): Double = sum(isPar) / tolNum

    def nanmean(isPar: Boolean = false): Double = nansum(isPar) / notNanNum

    //for n-order center moment . variance is the  "2-order center moment"
    def ocm(order: Int, isPar: Boolean = false): Double = {
      val meanVal: Double = mean(isPar)
      val f = (x: T) => x - meanVal
      baseCalc.sumWith(data, f, isPar) / tolNum
    }

    def nanocm(order: Int, isPar: Boolean = false): Double = {
      val meanVal: Double = mean(isPar)
      val f = (x: T) => x - meanVal
      baseCalc.nansumWith(data, f, isPar) / notNanNum
    }

  }

  class LiteralSeries(source: Vector[Any], index: Boolean = false) extends Series[String] with countable[String] {
    var name: String = ""
    val data: Vector[String] =
      if (source.length == 0) {
        name = "empty"; Vector[String]()
      }
      else {
        (index match {
          case true =>
            name = source.head.toString; source.tail
          case false => name = "unname"; source
        }) then (_.map(_.toString))
      }
    //I feel sooooooooooooooooooooo charming!
  }

  class NumericSeries[T <% Double : Numeric : ClassTag](source: Vector[Any], index: Boolean = false) extends Numerical[T] with countable[T] {
    var name: String = ""
    val data: Vector[T] = if (source.length == 0) {
      name = "empty"; Vector[T]()
    }
    else
      (index match {
        case true =>
          name = source.head.toString; source.tail
        case false => name = "unname"; source
      }) then {
        classTag[T] match {
          case ClassTag.Int => _.map(_.toString then baseCalc.Str2Int).asInstanceOf[Vector[T]]
          case ClassTag.Double => _.map(_.toString then baseCalc.Str2Double).asInstanceOf[Vector[T]]
          case ClassTag.Long => _.map(_.toString then baseCalc.Str2Long).asInstanceOf[Vector[T]]
        }
      }

  }

}
 
  

