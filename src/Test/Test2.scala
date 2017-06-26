package Test
import scala.collection.mutable.IndexedSeq
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
		val (c,d)=(1,1)
			//time(500)(run2)
	//	time(500)(run1)
		
		
		
		
  def main(args:Array[String]){
  import linalg.matLib.Matrix
  	val mat = new Matrix( IndexedSeq(IndexedSeq(1,5),IndexedSeq(3,2) ))
    val c= mat.c
    println(c)
    val (row,col)=(2,2)
      var tmp = c.head.head
       for(i <-0 until row)
         for(j <- i+1 until col)
         {
            tmp = c apply(j) apply(i)
            println(tmp,c apply i apply j )
           c (j) update (i,  c apply i apply j)
           c apply i update (j, tmp)
         }
    println(c)
  }
}