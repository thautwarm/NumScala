package Test
import scala.collection.mutable.{IndexedSeq=>seq}

object Test2 {
  
  	def run1(){
		val a=Vector.range(0, 30000)
		for (i <- 0 until a.length)
			a(i)
		}
	  def run2(){
		val a=IndexedSeq.range(0, 30000)
		for (i <- 0 until a.length)
			a(i)
		}
		
			//time(500)(run2)
	//	time(500)(run1)
		
		
//				val mat = new Matrix( IndexedSeq(IndexedSeq(1,5),IndexedSeq(3,2) ))
//                                                  //> mat  : linalg.matLib.Matrix[Int] = linalg.matLib$Matrix@4b1c1ea0
//       
//    val c= mat.c                                  //> c  : scala.collection.mutable.IndexedSeq[scala.collection.mutable.IndexedSeq
//                                                  //| [Int]] = ArrayBuffer(ArrayBuffer(1, 5), ArrayBuffer(3, 2))
//
//
//    mat.transpose()
//    
//   mat.storeWithIndex()                           //> res2: Boolean = true
//    mat.c                                         //> res3: scala.collection.mutable.IndexedSeq[scala.collection.mutable.IndexedSe
//                                                  //| q[Int]] = ArrayBuffer(ArrayBuffer(1, 3), ArrayBuffer(5, 2))
//   	mat.row                                   //> res4: Int = 2
//   	mat.col                                   //> res5: Int = 2
    
		
		
  def main(args:Array[String]){
  import linalg.matLib.Matrix
  import linalg.matLib.NumScalaGetManager
  val Get = NumScalaGetManager("method1")

  val a = new Matrix[Int](seq(seq(1,2,3,4,5),seq(6,7,8,9,10),seq(-1,-2,-3,-4,-5)))
  
  println(a.loc(Get,1,1))
  println(a.selectData(0, 0))
  println(a.c)
  
//  	val mat = new Matrix( IndexedSeq(IndexedSeq(1,5),IndexedSeq(3,2) ))
//    val c= mat.c
//    println(c)
//    val (row,col)=(2,2)
//      var tmp = c.head.head
//       for(i <-0 until row)
//         for(j <- i+1 until col)
//         {
//            tmp = c apply(j) apply(i)
//            println(tmp,c apply i apply j )
//           c (j) update (i,  c apply i apply j)
//           c apply i update (j, tmp)
//         }
//    println(c)
  
 	   
		}
}
