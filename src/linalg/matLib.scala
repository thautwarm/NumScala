package linalg
import scala.language.implicitConversions
import util.config._
import scala.collection.mutable.IndexedSeq
import scala.collection.parallel.ParSet
import scala.collection.GenSeq
object matLib {
  
  def GetShape[T](mat:Matrix[T]):(Int,Int)={
    (mat.row,mat.col)
  }
  def GetShape[T,VECS<%IndexedSeq[IndexedSeq[T]]](vecs:VECS):(Int,Int)={
    (vecs.length,vecs.head.length)
  }

  
    class Matrix[T](source: IndexedSeq[IndexedSeq[T]], checkAndPar: (Boolean, Boolean) = (true, true)) {
    val c :IndexedSeq[IndexedSeq[T]]= source
    private var (check, isParCons) = checkAndPar
    private def Row() = c.length
    private def Col()  = if (Row == 0) 0
    else
      check match {
        case false => c.head.length
        case true =>
          { check = false
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
    private var XIndex = IndexedSeq.range(0, Row)
    private var YIndex = IndexedSeq.range(0, Col)
    private var indexByRow = true
    
    def  isByRow() = indexByRow
    
    def row() = if (indexByRow) Row else Col
    def col() = if (indexByRow) Col else Row
    
    def apply(idx:Int):IndexedSeq[T]={
      c apply idx
    }
    
    
    // selectData from the source data "c"
    
    // BEGIN DEF selectData
    private def selectData[INDEX<%IndexedSeq[Int]](Rows:INDEX,Cols:INDEX):IndexedSeq[IndexedSeq[T]]=
            Rows map {rowidx=>
                val RowData = c apply rowidx
                val RowDealer = (colidx:Int)=>RowData apply colidx
                Cols map RowDealer 
             }
    
    private def selectData[INDEX<%IndexedSeq[Int]](RowIdx:Int, Cols:INDEX):IndexedSeq[T]={
          val RowData = c apply RowIdx
          Cols map (RowData apply _)
      }
    private def selectData[INDEX<%IndexedSeq[Int]](Rows:INDEX, ColIdx:Int):IndexedSeq[T]={
          Rows map (rowidx  => c apply rowidx apply ColIdx)
      }
    
    private def selectData(RowIdx:Int,ColIdx:Int):T={
          c apply RowIdx apply ColIdx
    }
    
    //END DEF selectData
    
    
    // updateData from the source data "c"
    
    // BEGIN DEF updateData
    private def updateData(RowIdx:Int, ColIdx:Int,changeValue:T):Unit={ //1 1 
          c apply RowIdx update (ColIdx,changeValue)
    }
    
    
    private def updateData[INDEX<%IndexedSeq[Int]]
    (RowIdx:Int, Cols:INDEX,changeValue:T):Unit={  //1 n 1
          val RowData = c apply RowIdx
          Cols map (RowData update (_,changeValue) )
    }
    
    private def updateData[INDEX<%IndexedSeq[Int]] // n 1 1
    (Rows:INDEX, ColIdx:Int,changeValue:T):Unit={
          Rows map {rowidx=>c apply rowidx update (ColIdx, changeValue)}
    }
    
    private def updateData[INDEX<%IndexedSeq[Int],VEC<%IndexedSeq[T]] // 1 n n
      (RowIdx:Int, Cols:INDEX,changeValue:VEC):Unit={
          val actions = for{i<- Iterator.range(0,changeValue.length) } yield i
          val RowData = c apply RowIdx
          Cols map (RowData update (_,changeValue apply actions.next) )
      }
    
    private def updateData[INDEX<%IndexedSeq[Int],VEC<%IndexedSeq[T]]  // n 1 n
      (Rows:INDEX, ColIdx:Int,changeValue:VEC):Unit={
          val actions = for{i<- Iterator.range(0,changeValue.length) } yield i
          Rows map (c apply ColIdx update (_,changeValue apply actions.next) )
      }
    private def updateData[INDEX<%IndexedSeq[Int]]
      (Rows:INDEX, Cols:INDEX,changeValue:Matrix[T]):Unit={
          var x_idx,y_idx= (-1)
          val actions =  for{i<- Iterator.range(0,changeValue.row)
                             j<- Iterator.range(0, changeValue.col)} yield j   
          Rows map{rowidx=>
            val RowData = c apply rowidx
            val changeToRow = changeValue apply (x_idx+1)  //...............
            x_idx+=1
            Cols map {RowData update (_,changeToRow apply actions.next() )} 
          }
      }    
    private def updateData[INDEX<%IndexedSeq[Int],VECS<%IndexedSeq[IndexedSeq[T]]]
      (Rows:INDEX, Cols:INDEX,changeValue:VECS):Unit={
          var x_idx,y_idx= (-1)
          val actions =  for{i<- Iterator.range(0,changeValue.length)
                             j<- Iterator.range(0, changeValue.head.length)} yield j   
          Rows map{rowidx=>
            val RowData = c apply rowidx
            val changeToRow = changeValue apply (x_idx+1)  //...............
            x_idx+=1
            Cols map {RowData update (_,changeToRow apply actions.next() )} 
          }
      }   


    
    private def updateData[INDEX<%IndexedSeq[Int]]
      (Rows:INDEX, Cols:INDEX,changeValue:T):Unit={
          var x_idx,y_idx=0
          Rows map{rowidx=>
            val RowData = c apply rowidx
            Cols map {RowData update (_,changeValue)} 
          }
      }
    
//    
//    
//    private def updateData[INDEX<%IndexedSeq[Int]](Rows:INDEX, Cols:Int,changeValue:T):Unit={
//          val RowData = c apply RowIdx
//          Cols map (RowData update (_,changeValue) )
//    }    
    

    
    //====================4 kinds of loc-func as follow:=======================
    
    
    // set a matrix-like to a part of this one.
    
    // BEGIN GET_OR_SET_MATRIX as (VEC,VEC)
    // GET
    def loc[INDEX<%IndexedSeq[Int]]
    (rows: INDEX, cols: INDEX): IndexedSeq[IndexedSeq[T]] = {
      if (indexByRow) selectData(rows,cols)
      else            selectData(cols,rows)
    }
    
    
    // SET from  Matrix[T]
    def loc[INDEX<%IndexedSeq[Int]]
    (rows: INDEX, cols: INDEX)(changeValue:Matrix[T]): Unit = {
      /*     This is the origin version 
 			*     
      {
      val ijs =  
        for { i <-Iterator.range(0, changeValue.row);j<-Iterator.range(0, changeValue.col)}
          yield (i,j)
      val unitDo =  
        (rowidx:Int,colidx:Int)=>
          {val (i,j) = ijs.next();setter(rowidx)(colidx)(changeValue.loc(i,j))}
      }	
		* 
		*/     

      /*the optimized version is in the method updateData(INDEX,INDEX,MATRIX[T])*/
      if (indexByRow) updateData(rows, cols, changeValue)
      else            updateData(cols, rows, changeValue)
    }
    
    // SET from  IndexedSeq[IndexedSeq[T]]
    def loc[INDEX<%IndexedSeq[Int],MAT<%IndexedSeq[IndexedSeq[T]]]
    (rows: INDEX, cols: INDEX)(changeValue:MAT): Unit = {
      if (indexByRow) updateData(rows,cols, changeValue)
      else            updateData(cols,rows, changeValue)
    }
    def loc[INDEX<%IndexedSeq[Int]]
    (rows: INDEX, cols: INDEX)(changeValue:T): Unit = {
      if (indexByRow) updateData(rows,cols, changeValue)
      else            updateData(cols,rows, changeValue)
    }
    
    
    // END  GET_OR_SET_MATRIX as (VEC,VEC)
    
    
    // BEGIN GET_OR_SET_MATRIX as (Int,VEC)
    
    //GET
    def loc[INDEX<%IndexedSeq[Int]]
    (rowidx: Int, cols: INDEX ): IndexedSeq[T] = {
      if (indexByRow) selectData(rowidx,cols)
                 else selectData(cols,rowidx)
    }
    
    //SET
    def loc[INDEX<%IndexedSeq[Int],VEC<%IndexedSeq[T]]
    (rowidx: Int, cols: INDEX )(changeValue:VEC): Unit = {
      if (indexByRow) updateData(rowidx, cols, changeValue)
                 else updateData(cols, rowidx, changeValue)
    }
    
    def loc[INDEX<%IndexedSeq[Int]]
    (rowidx: Int, cols: INDEX )(changeValue:T): Unit = {
      if (indexByRow) updateData(rowidx, cols, changeValue)
                 else updateData(cols, rowidx, changeValue)
    }
    // END  GET_OR_SET_MATRIX as (Int,VEC)
    
    // BEGIN GET_OR_SET_MATRIX as (VEC,Int)
    
    //GET
    def loc[INDEX<%IndexedSeq[Int]]
    (rows: INDEX, colidx: Int): IndexedSeq[T] = {
      if (indexByRow) selectData(rows, colidx)
                 else selectData(colidx, rows)
    }
    
    //SET
    
    def loc[INDEX<%IndexedSeq[Int],VEC<%IndexedSeq[T]]
    (rows:INDEX, colidx: Int )(changeValue:VEC): Unit = {
      if (indexByRow) updateData(rows, colidx, changeValue)
                 else updateData(colidx, rows, changeValue)
    }
    
    def loc[INDEX<%IndexedSeq[Int]]
    (rows:INDEX, colidx: Int )(changeValue:T): Unit = {
      if (indexByRow) updateData(rows, colidx, changeValue)
                 else updateData(colidx, rows, changeValue)
    }
    
    // END  GET_OR_SET_MATRIX as (VEC,Int) 
    

    // BEGIN GET_OR_SET_MATRIX as (Int,Int)
    
    //GET
    def loc(rowidx: Int, colidx: Int): T = {
      if (indexByRow) selectData(rowidx, colidx)
                 else selectData(colidx, rowidx)
    }
    
    //SET
    def loc(rowidx: Int, colidx: Int)(changeValue:T): Unit= {
      if (indexByRow) updateData(rowidx, colidx, changeValue)
                 else updateData(colidx, rowidx, changeValue)
    }
    // END  GET_OR_SET_MATRIX as (Int,Int)
    
    
    /* transpose  */
    def transpose(): Unit = { //transpose and do not return
      indexByRow = !indexByRow
      
    }
    def  T():Matrix[T]={    // transpose and return 
      indexByRow = !indexByRow
      this
    }
    
    def cp():Matrix[T]={   //get the copy of this matrix
      val ret = new Matrix[T](this.c.map( _.map(x=>x) ),(false,false))
      ret.indexByRow = this.indexByRow
      ret
    }
   
    /* conform the ways about storing data and indexing data  */
    def storeWithIndex(): Boolean = {   
      if (!indexByRow) {
          var tmp = c.head.head
          
          val inv:(Int,Int)=>Unit = (i:Int,j:Int) => 
            {tmp = c apply (j) apply (i)
                c apply j update (i, c apply i apply j)
                c apply i update (j, tmp) }
        
          for{ i<- 0 until row  ; j <- i+1 until col } inv(i,j)
          indexByRow = true
          true} 
      else false

    }

  }
}