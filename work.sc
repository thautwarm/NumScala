import scala.language.implicitConversions
import util.config._
import scala.compat.Platform.currentTime
import scala.collection.mutable.IndexedSeq
import util.baseCalc._

object work{
 import scala.compat.Platform.currentTime
   def time(cnt: Int)(call : => Unit): Long = {
          val start = System.currentTimeMillis
          (1 to cnt) foreach (_ => call)
          
           System.currentTimeMillis - start
       }                                          //> time: (cnt: Int)(call: => Unit)Long
	
	import util.config.{NSInt,NSLong}
	Vector.range(0,2)                         //> res0: scala.collection.immutable.Vector[Int] = Vector(0, 1)
	
	import linalg.matLib.Matrix
	val mat = new Matrix( IndexedSeq(IndexedSeq(1,5),IndexedSeq(3,2) ))
                                                  //> mat  : linalg.matLib.Matrix[Int] = linalg.matLib$Matrix@6acdbdf5
    val c= mat.c                                  //> c  : scala.collection.mutable.IndexedSeq[scala.collection.mutable.IndexedSeq
                                                  //| [Int]] = ArrayBuffer(ArrayBuffer(1, 5), ArrayBuffer(3, 2))


    mat.transpose()
   mat.storeWithIndex()
    mat.c                                         //> res1: scala.collection.mutable.IndexedSeq[scala.collection.mutable.IndexedSe
                                                  //| q[Int]] = ArrayBuffer(ArrayBuffer(1, 3), ArrayBuffer(5, 2))
    

                

		
	
		
		
		
		
}