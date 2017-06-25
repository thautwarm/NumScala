import scala.language.implicitConversions
import util.config._
import util.baseCalc.tools.sumWith
import scala.compat.Platform.currentTime
object work{;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(198); 
		val a:Vector[Int]=Vector.range(0, 200000, 3);System.out.println("""a  : Vector[Int] = """ + $show(a ));$skip(58); 
		
		def run(t:Boolean)={
		sumWith(a,(x:Int)=>x+1,t)
		};System.out.println("""run: (t: Boolean)Int""");$skip(21); 
		var t= currentTime;System.out.println("""t  : Long = """ + $show(t ));$skip(12); val res$0 = 
		run(true);System.out.println("""res0: Int = """ + $show(res$0));$skip(25); 
		println(t-currentTime)}
		

	
		 
		 
		 
		
		

		
		
		
         

}
