package util
import scala.io.Source
import scala.collection.mutable
import math._
import scala.collection.LinearSeq
import scala.annotation.tailrec
import scala.collection.parallel
object baseCalc {    
    def sum[T:Numeric](iter:Vector[T],parallel:Boolean=false):T={
      if (parallel)  iter.par.sum else iter.sum
    }
  
    def sumWith[T:Numeric](iter: Vector[T], kernel: T => T, parallel:Boolean=false): T = 
      if (parallel)  iter.par.map(kernel(_)).sum
      else iter.toIterable.map(kernel(_)).sum
      
    def map[T](vec:Vector[T],func:T=>T,parallel:Boolean=false)={
        if (parallel) vec.par.map(func(_)).toVector else vec.map(func(_)).toVector
      }
    
 
}