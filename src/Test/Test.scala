package Test
object Test extends App {
//  val path = raw"H:\Kaggle\TitanicDisaster\test.csv"
//  val all = csv.read(path)
//  val Source = all.map(_(2)).toVector
//  println(Source.slice(0, 5))
//  val tmp = Source.tail.+:(Source(0)).toVector
//  println(tmp(2))
//  val series = new LiteralSeries(tmp, true)
//  println(series.name)
//  println(series.data(0).getClass)
//  println(series.count(_.contains("Mr.") ))
//  println(series.frequency(_.length/10))
  
  import user.dataFrame.{NumericSeries,LiteralSeries}
  import util.fileDeal.csv 
  import util.config._
  import util.baseCalc.Str2Int
  import scala.language.implicitConversions
  
  val path = raw"H:\Kaggle\TitanicDisaster\train.csv"
  val all = csv.read(path)
  val Source1 = all.map(_(4)).toVector
  val Source2 = all.map(_(5)).toVector
  val tmp1 = Source1.tail.+:(Source1(0)).toVector
  val tmp2 = Source2.tail.+:(Source2(0)).toVector
  println(tmp1.slice(0, 5))
  println(tmp2.slice(0, 5))
  
  val series1 = new LiteralSeries(source=tmp1, index=true)
  val series2 = new NumericSeries[Double](source=tmp2, index=true)
  println("========Series1=========")
  println(s"Name: ${series1.name}")
  println(s"how many male survived(in train datasets)  ${series1.count(_=="male")}")
  println(s"how many NaN records: ${series1.count(_==null)}")
  println(s"the distribution of the values:\n\t ${series1.unique}")
  println(s"the frequency distributions:\n\t ${series1.frequency((x:String)=>x).mkString("\n")}")
  println("========Series2=========")
  println(s"Name: ${series2.name}")
  println(s"total: ${series2.tolNum}")
  println(s"NaN: ${series2.nanNum}")
  println(s"variance : ${series2.ocm(2,isPar=true)}")
  println(s"mean : ${series2.mean(isPar=true)}")
  println(s"group the people with ages of 10*n : \n${series2.frequency(
                    (x)=>s"AgeIn: ${ (x/10).toInt then ((x)=>(10*x,10*(x+1) )) }").mkString("\n")}")
  
  
  //println(Source.slice(0, 5))
  //  println(series.count(_.contains("Mr.") ))
//  println(series.frequency(_.length/10))  
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