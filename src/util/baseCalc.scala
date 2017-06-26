package util
import scala.io.Source
import scala.collection.mutable
import math._
import scala.collection.LinearSeq
import scala.annotation.tailrec
import scala.collection.parallel
import javax.print.attribute.standard.MediaSize.NA

object baseCalc {
  def sum[T: Numeric](iter: Vector[T], parallel: Boolean = false): T = {
    if (parallel) iter.par.sum else iter.sum
  }
  def nansum[T: Numeric](iter: Vector[T], parallel: Boolean = false): T = {
    if (parallel) iter.filter(_ != null).par.sum else iter.filter(_ != null).sum
  }

  def sumWith[T: Numeric, G: Numeric](iter: Vector[T], kernel: T => G, parallel: Boolean = false): G =
    if (parallel) iter.par.map(kernel(_)).sum.asInstanceOf[G]
    else iter.toIterable.map(kernel(_)).sum.asInstanceOf[G]

  def nansumWith[T: Numeric, G: Numeric](iter: Vector[T], kernel: T => G, parallel: Boolean = false): G =
    if (parallel) iter.par.filter(_ != null).map(kernel(_)).sum.asInstanceOf[G]
    else iter.toIterable.filter(_ != null).map(kernel(_)).sum.asInstanceOf[G]

  def map[T,G](vec: Vector[T], func: T => G, parallel: Boolean = false):Vector[G] = {
    if (parallel) vec.par.map(func(_)).toVector else vec.map(func(_)).toVector
  }

  def nanmap[T,G](vec: Vector[T], func: T => G, parallel: Boolean = false):Vector[G] = {
    if (parallel) vec.par.filter(_ != null).map(func(_)).toVector else vec.filter(_ != null).map(func(_)).toVector
  }

  def splitAt(str: String, idx: Int): (String, String) = {
    (str.substring(0, idx), str.substring(idx + 1))
  }

  def Str2Double(str: String): Any = {
    str match {
      case "" => null
      case _  => str.toDouble
    }

  }

  def Str2Long(str: String): Any = {
    str match {
      case "" => null
      case _  => str.toLong
    }
  }
  def Str2Int(str: String): Any = {
    str match {
      case "" => null
      case _  => str.toInt
    }
  }
  def Str2Float(str: String): Any = {
    str match {
      case "" => null
      case _  => str.toFloat
    }
  }

}