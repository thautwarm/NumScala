package util
object config {
  
  case class NumScalaType[T](arg:String)
  val NSInt=NumScalaType[Int]("Int")
  val NSDouble=NumScalaType[Double]("Double")
  val NSString=NumScalaType[String]("String")
  val NSLong=NumScalaType[Long]("Long")
  
  def Dict[K, V](keys: Seq[K], values: Seq[V]) = (keys zip values) toMap

  
  implicit class __middleFunc__[T](arg: T) {
    def then[G](f: T => G): G = {
      f(arg)
    }

  }

}

 