import scala.language.implicitConversions
import util.config._
import scala.compat.Platform.currentTime
import scala.collection.mutable.IndexedSeq
import util.baseCalc._

object work{
 import scala.compat.Platform.currentTime;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(425); 
   def time(cnt: Int)(call : => Unit): Long = {
          val start = System.currentTimeMillis
          (1 to cnt) foreach (_ => call)
          
           System.currentTimeMillis - start
       }
	
	import util.config.{NSInt,NSLong};System.out.println("""time: (cnt: Int)(call: => Unit)Long""");$skip(56); val res$0 = 
	Vector.range(0,2)
	
	import linalg.matLib.Matrix;System.out.println("""res0: scala.collection.immutable.Vector[Int] = """ + $show(res$0));$skip(100); 
	val mat = new Matrix( IndexedSeq(IndexedSeq(1,5),IndexedSeq(3,2) ));System.out.println("""mat  : linalg.matLib.Matrix[Int] = """ + $show(mat ));$skip(17); 
    val c= mat.c;System.out.println("""c  : scala.collection.mutable.IndexedSeq[scala.collection.mutable.IndexedSeq[Int]] = """ + $show(c ));$skip(22); 


    mat.transpose();$skip(24); 
   mat.storeWithIndex();$skip(10); val res$1 = 
    mat.c;System.out.println("""res1: scala.collection.mutable.IndexedSeq[scala.collection.mutable.IndexedSeq[Int]] = """ + $show(res$1))}
    

                

		
	
		
		
		
		
}
