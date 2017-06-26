package Test
import user.dataFrame.{NumberSeries,LiteralSeries}
import util.fileDeal.csv
import util.config.{ NSInt, NSLong, NSDouble, NSString }
object Test extends App {
  val path = raw"H:\Kaggle\TitanicDisaster\test.csv"
  val all = csv.read(path)
  val Source = all.map(_(2)).toVector
  println(Source.slice(0, 5))
  val tmp = Source.tail.+:(Source(0)).toVector
  println(tmp(2))
  val series = new LiteralSeries(tmp, true)
  println(series.name)
  println(series.data(0).getClass)
  println(series.count(_.contains("Mr.") ))
  println(series.frequency(_.length/10))
  
//  println(series.nanmean(true))
//  println(series.nansum(true))
//  println(series.nanocm(2, true))
  // import scala.compat.Platform.currentTime
  // def time(cnt: Int)(call : => Unit): Long = {
  //        val start = System.currentTimeMillis
  //        (1 to cnt) foreach (_ => call)
  //        
  //         System.currentTimeMillis - start 
  //      }
  //   
  ////      
  ////     val a=Vector.range(0,10000000,1)
  ////     println( time(10)(a map (_+1)) )
  ////     println( time(10)(a.par map(_+1)) )

  //> foo: [T](l: List[T])(implicit evidence$2: Numeric[T])Unit

}