package util
import scala.io.Source
import scala.collection.mutable
import math._
import scala.collection.LinearSeq
import scala.annotation.tailrec
import scala.collection.parallel
import oracle.jrockit.jfr.parser.SubStruct

object baseCalc {    
    def sum[T:Numeric](iter:Vector[T],parallel:Boolean=false):T={
      if (parallel)  iter.par.sum else iter.sum
    }
    
    def nansum[T:Numeric](iter:Vector[T],parallel:Boolean=false):T={
      if (parallel)  iter.filter(_!=null).par.sum else iter.filter(_!=null).sum
    } 

    def sumWith[T:Numeric](iter: Vector[T], kernel: T => T, parallel:Boolean=false): T = 
      if (parallel)  iter.par.map(kernel(_)).sum
      else iter.toIterable.map(kernel(_)).sum
      
    def nansumWith[T:Numeric](iter: Vector[T], kernel: T => T, parallel:Boolean=false): T = 
      if (parallel)  iter.par.filter(_!=null).map(kernel(_)).sum
      else iter.toIterable.filter(_!=null).map(kernel(_)).sum
      
    def map[T](vec:Vector[T],func:T=>T,parallel:Boolean=false)={
        if (parallel) vec.par.map(func(_)).toVector else vec.map(func(_)).toVector
      }
    
    def nanmap[T](vec:Vector[T],func:T=>T,parallel:Boolean=false)={
        if (parallel) vec.par.filter(_!=null).map(func(_)).toVector else vec.filter(_!=null).map(func(_)).toVector
      }
    
    def splitAt(str:String,idx:Int):(String,String)={
      (str.substring(0, idx),str.substring(idx+1))
    }
 
}