package util
object config {
  implicit class __middleFunc__[T](arg: T) {
    def then[G](f: T => G): G = {
      f(arg)
    }
  }

}

 