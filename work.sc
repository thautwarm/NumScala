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
       }
	
	scala.math.Integral
	
	import linalg.matLib.Matrix
	
	val a = new Matrix[Double](seq(seq(1,2,5,6,10),seq(0,5,9,20,-5)))
	a.loc(1,1)
	a.loc(15)(1,1)
	a.loc(1,1)
		
		
}