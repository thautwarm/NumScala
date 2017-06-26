package linalg
import scala.language.implicitConversions
import util.config._
import scala.collection.mutable.IndexedSeq
import scala.collection.parallel.ParSet
object matLib {
  class Matrix[T](source: IndexedSeq[IndexedSeq[T]], checkAndPar: (Boolean, Boolean) = (true, true)) {
    var c = source
    var (check, isParCons) = checkAndPar
    private def Row = c.length
    private def Col = if (Row == 0) 0
    else
      check match {
        case false => c.head.length
        case true =>
          {
            isParCons match {
              case false =>
                c.map(_.length).toSet then
                  { (s: Set[Int]) => if (s.size > 1) -1 else s.head }
              case true =>
                c.par.map(_.length).toSet then
                  { (s: ParSet[Int]) => if (s.size > 1) -1 else s.head }
            }
          }
      }
    var XIndex = IndexedSeq.range(0, Row)
    var YIndex = IndexedSeq.range(0, Col)
    var indexByRow = true

    def row() = if (indexByRow) Row else Col
    def col() = if (indexByRow) Col else Row
    
    //4 kinds of loc-func as follow:
    def loc(rows: IndexedSeq[Int], cols: IndexedSeq[Int]): IndexedSeq[IndexedSeq[T]] = {
      if (indexByRow) rows map {
        val Row = c apply (_)
        cols map (Row apply (_))
      }
                 else cols map {
        val Row = c apply (_)
        rows map (Row apply (_))
      }
    }
    
    def loc(rowidx: Int, cols: IndexedSeq[Int]): IndexedSeq[T] = {
      if (indexByRow) cols.map(c apply (rowidx) apply (_))
                 else cols.map(c apply (_) apply (rowidx))
    }
    
    def loc(rows: IndexedSeq[Int], colidx: Int): IndexedSeq[T] = {
      if (indexByRow) rows map (c apply (_) apply (colidx))
                 else rows map (c apply (colidx) apply (_))
    }
    
    def loc(rowidx: Int, colidx: Int): T = {
      if (indexByRow) c apply (rowidx) apply (colidx)
                 else c apply (colidx) apply (rowidx)
    }
    
    
    /* transpose  */
    def transpose(): Unit = {
      indexByRow = !indexByRow
    }
    def T()=transpose()
    
    
    /* conform the ways about storing data and indexing data  */
    def storeWithIndex(): Boolean = {
      if (!indexByRow) {
        var tmp = c.head.head
        for (i <- 0 until row)
          for (j <- i + 1 until col) {
            tmp = c apply (j) apply (i)
            c apply j update (i, c apply i apply j)
            c apply i update (j, tmp)
          }
        true} else false

    }
  }
}