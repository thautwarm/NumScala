import scala.language.implicitConversions
import util.config._
import scala.compat.Platform.currentTime
import scala.collection.mutable.{IndexedSeq=>seq}
import util.baseCalc._
import scala.reflect.{ClassTag,classTag}
object work{
 import scala.compat.Platform.currentTime;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(472); 
   def time(cnt: Int)(call : => Unit): Long = {
          val start = System.currentTimeMillis
          (1 to cnt) foreach (_ => call)
          
           System.currentTimeMillis - start
       };System.out.println("""time: (cnt: Int)(call: => Unit)Long""");$skip(23); val res$0 = 
	
	scala.math.Integral
	
	import linalg.matLib.Matrix;System.out.println("""res0: math.Integral.type = """ + $show(res$0));$skip(100); 
	
	val a = new Matrix[Double](seq(seq(1,2,5,6,10),seq(0,5,9,20,-5)));System.out.println("""a  : linalg.matLib.Matrix[Double] = """ + $show(a ));$skip(12); val res$1 = 
	a.loc(1,1);System.out.println("""res1: Double = """ + $show(res$1));$skip(16); val res$2 = 
	a.loc(15)(1,1);System.out.println("""res2: <error> = """ + $show(res$2));$skip(12); val res$3 = 
	a.loc(1,1);System.out.println("""res3: Double = """ + $show(res$3))}
		
		
}
