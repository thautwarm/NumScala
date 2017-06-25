package user
import util.config.Dict
import util.config.{ DoubleClass, IntegerClass, FloatClass, StringClass, LongClass }
import util.baseCalc
import math.pow
case class Numeric[T](arg: scala.Numeric[T])
object dataFrame {

  class Series[T](source: Vector[Any], index: Boolean = false) {

    val name: String =
      if (source.length == 0) null else {
        if (index) source(0).toString else "unname"
      }
    val data: Vector[T] = (source.slice(1, source.length)).asInstanceOf[Vector[T]]
    val Type = data(0).getClass()
    val status = {
      Type match {
        case DoubleClass  => true
        case IntegerClass => false
        case FloatClass   => true
        case LongClass    => true
        case _            => null
      }
    }
    val sum = (isPar: Boolean) =>
      status match {
        case null => () => null
        case true =>
          {
            def lambda() =
              { baseCalc.sum(this.data.asInstanceOf[Vector[Double]], isPar) }
          }
        case false =>
          {
            def lambda() =
              { baseCalc.sum(this.data.asInstanceOf[Vector[Int]], isPar) }
          }
      }
    val mean = (isPar: Boolean) =>
      (status match {
        case null => null// () => { println("This Series is not Numeric!"); 0.0 }
        case true =>
          {
            def lambda() =
              { baseCalc.sum(this.data.asInstanceOf[Vector[Double]], isPar) / this.data.length }
            lambda
          }
        case false =>
          {
            def lambda() =
              { (1.0 * baseCalc.sum(this.data.asInstanceOf[Vector[Int]], isPar)) / this.data.length }
            lambda
          }
      }): () => Double
    val ocm = (order: Int, isPar: Boolean) =>
      (status match {
        case null => null
        case true =>
          {
            def lambda() =
              {
                val meanVal: Double = this.mean(isPar)()
                baseCalc.sumWith(
                  baseCalc.map(
                    this.data.asInstanceOf[Vector[Double]], (x: Double) => x - meanVal, isPar),
                  (x: Double) => pow(x, order),
                  isPar) / this.data.length
              }
            lambda
          }
        case false =>
          {
            def lambda() =
              {
                val meanVal: Double = this.mean(isPar)()
                baseCalc.sumWith(
                  baseCalc.map(
                    this.data.asInstanceOf[Vector[Int]].asInstanceOf[Vector[Double]], (x: Double) => x - meanVal, isPar),
                  (x: Double) => pow(x, order),
                  isPar) / this.data.length
              }
            lambda
          }

      }): () => Double
      
  }

}
  
//     class DataFrame(source:Any,config:Map[String,Any]=Map() )
 
  

