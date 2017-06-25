package Test
import scala.language.implicitConversions
import util.config._
import scala.compat.Platform.currentTime
import scala.language.implicitConversions
object Test extends App{
   def time(cnt: Int)(call : => Unit): Long = {
        val start = System.currentTimeMillis
        (1 to cnt) foreach (_ => call)
        
         System.currentTimeMillis - start 
      }
   
      
     val a=Vector.range(0,10000000,1)
     println( time(10)(a map (_+1)) )
     println( time(10)(a.par map(_+1)) )
     
     
    
		
}