import scala.language.implicitConversions
import util.config._
import util.baseCalc.tools.sumWith
import scala.compat.Platform.currentTime
object work{
		val a:Vector[Int]=Vector.range(0, 200000, 3)
		
		def run(t:Boolean)={
		sumWith(a,(x:Int)=>x+1,t)
		}
		var t= currentTime
		run(true)
		println(t-currentTime)
		

	
		 
		 
		 
		
		

		
		
		
         

}