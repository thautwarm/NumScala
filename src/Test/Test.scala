package Test
import scala.language.implicitConversions
import user.dataFrame.Series
import util.config._
import util.fileDeal.csv
object Test extends App{
   val path= raw"H:\Kaggle\TitanicDisaster\test.csv"
   val all = csv.read(path)
   println(all(0).toList)
   val source = all.map(_(4)).toVector
   val f = (x:Any)=>if (x=="") null else x.toString.toDouble
   println(source.slice(0, 5))
   val tmp =  source.tail.map( f(_) ).asInstanceOf[Vector[Any]] .+:(source(0))
   println(tmp(2) )
   
   val series = new Series[Int]( tmp.toVector,index=true)
   println(series.Type)
   println(series.name)
   println(series.nanmean(true)())
   println(series.nansum(true)())
   println(series.nanocm(order=2,true)())  // variance
   
 
 import scala.compat.Platform.currentTime
 def time(cnt: Int)(call : => Unit): Long = {
        val start = System.currentTimeMillis
        (1 to cnt) foreach (_ => call)
        
         System.currentTimeMillis - start 
      }
   
//      
//     val a=Vector.range(0,10000000,1)
//     println( time(10)(a map (_+1)) )
//     println( time(10)(a.par map(_+1)) )
     
     
    
		
}