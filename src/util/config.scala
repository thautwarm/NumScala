package util
object config {

  val DoubleClass = new java.lang.Double(0.0).getClass 
  val IntegerClass = new java.lang.Integer(0).getClass
  val LongClass = new java.lang.Long(0).getClass
  val FloatClass = new java.lang.Float(10e0).getClass
  val StringClass = new java.lang.String("").getClass
  
  def  Dict[K,V](keys:Seq[K], values:Seq[V]) =  (keys zip values) toMap
   
  implicit class middleFunc[T](arg:T){
      def then[G](f:T=>G){
        f(arg)
      }
    
  }
   

}

 