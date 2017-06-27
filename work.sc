import scala.language.implicitConversions
import util.config._
import scala.compat.Platform.currentTime
import scala.collection.mutable.{IndexedSeq,ArraySeq}
import util.baseCalc._
import scala.reflect.{ClassTag,classTag}
object work{
 import scala.compat.Platform.currentTime
   def time(cnt: Int)(call : => Unit): Long = {
          val start = System.currentTimeMillis
          (1 to cnt) foreach (_ => call)
          
           System.currentTimeMillis - start
       }                                          //> time: (cnt: Int)(call: => Unit)Long
	
	scala.math.Integral                       //> res0: math.Integral.type = scala.math.Integral$@75bd9247
	
	import util.config.{NSInt,NSLong}
	Vector.range(0,2)                         //> res1: scala.collection.immutable.Vector[Int] = Vector(0, 1)
	
    def f[T:ClassTag](arg:Any){
       classTag[T] match{
        case ClassTag.Int=>println(arg.asInstanceOf[Int])
      }
      }                                           //> f: [T](arg: Any)(implicit evidence$1: scala.reflect.ClassTag[T])Unit
    f[Int](1)                                     //> 1
      
	
		
		
}