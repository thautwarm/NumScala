import scala.language.implicitConversions
import util.config._
import scala.compat.Platform.currentTime
import scala.collection.mutable.{IndexedSeq=>seq}
import util.baseCalc._
import scala.reflect.{ClassTag,classTag}
object work{
 import scala.compat.Platform.currentTime
   def time(cnt: Int)(call : => Unit): Long = {
          val start = System.currentTimeMillis
          (1 to cnt) foreach (_ => call)
          
           System.currentTimeMillis - start
       }                                          //> time: (cnt: Int)(call: => Unit)Long
	
	scala.math.Integral                       //> res0: math.Integral.type = scala.math.Integral$@c818063
	
  import linalg.matLib.Matrix
  val a = new Matrix[Int](seq(seq(1,2,5,6,10),seq(0,5,9,20,-5)))
                                                  //> a  : linalg.matLib.Matrix[Int] = linalg.matLib$Matrix@7c29daf3
 	println(a.loc2d(seq(1,1),seq(1,1)))       //> ArrayBuffer(ArrayBuffer(5, 5), ArrayBuffer(5, 5))
		
}