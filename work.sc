import scala.collection.immutable.{Seq=>imSeq,Vector=>imVec,Queue=>imQ}
import scala.collection.mutable.{Seq=>mSeq,ArraySeq,Queue=>mQ,ListBuffer,LinearSeq,ArrayBuffer}
import scala.compat.Platform.currentTime
import java.io.{ File, FileInputStream, FileOutputStream, InputStream, OutputStream, IOException }
import scala.io.Source
import scala.annotation.tailrec
object work {
	val pass = null                           //> pass  : Null = null
  def TextParser(source: String, sep: Char = ','): Array[Array[Any]] = {
      val buffer = ArrayBuffer[ArrayBuffer[Any]]()
      val bufferString = Source.fromFile(source)

      def parse(row: String) = {
        var incell: Boolean = false
        var readyOut: Boolean = false
        val sepEnum = Set(sep, '\n')

        def dealWithQuote(str: String): String = {
          var getQuote = false
          val toRet =
            (for (chr <- str) yield {
              if (getQuote) {
                if (chr != '"') { getQuote = false; chr }
                else chr
              } else {
                if (chr != '"') chr
                else { getQuote = true; "" }
              }
            })
          toRet.mkString("")
        }

        def getSplitIndexOf(str: String): (Int, Boolean) = {
          var i = 0
          for (chr <- str) {
            if (chr == ' ') { pass };
            else if (!incell && chr == '"')
              incell = true
            else if (incell && readyOut && sepEnum.apply(chr))
              return (i, incell)
            else if (incell && (chr == '"'))
              readyOut = false
            i += 1
          }
          (-1, incell)
        }

        def halfSplit(str: String): (String, String) = {
          getSplitIndexOf(str) match {
            case (-1, incell) => (if (incell) dealWithQuote(str) else str, "")
            case (idx: Int, incell) => {
              val (parsed, unparsed) = str.splitAt(idx)
              (if (incell) dealWithQuote(parsed) else parsed, unparsed)

            }
          }
        }

        @tailrec
        def Go(str: String, res: ArrayBuffer[Any] = ArrayBuffer()): ArrayBuffer[Any] = {
          if (str == "") res
          else {
            val (parsed, unparsed) = halfSplit(str)
            Go(unparsed, res += parsed)
          }
        }

        Go(row)
      }

      bufferString.getLines().toArray.map(parse(_).toArray)

  }                                               //> TextParser: (source: String, sep: Char)Array[Array[Any]]
  val csv=TextParser("H:/Kaggle/TitanicDisaster/train.csv")
                                                  //> csv  : Array[Array[Any]] = Array(Array(PassengerId,Survived,Pclass,Name,Sex
                                                  //| ,Age,SibSp,Parch,Ticket,Fare,Cabin,Embarked), Array(1,0,3,Braund, Mr. Owen 
                                                  //| Harris,male,22,1,0,A/5 21171,7.25,,S), Array(2,1,1,Cumings, Mrs. John Bradl
                                                  //| ey (Florence Briggs Thayer),female,38,1,0,PC 17599,71.2833,C85,C), Array(3,
                                                  //| 1,3,Heikkinen, Miss. Laina,female,26,0,0,STON/O2. 3101282,7.925,,S), Array(
                                                  //| 4,1,1,Futrelle, Mrs. Jacques Heath (Lily May Peel),female,35,1,0,113803,53.
                                                  //| 1,C123,S), Array(5,0,3,Allen, Mr. William Henry,male,35,0,0,373450,8.05,,S)
                                                  //| , Array(6,0,3,Moran, Mr. James,male,,0,0,330877,8.4583,,Q), Array(7,0,1,McC
                                                  //| arthy, Mr. Timothy J,male,54,0,0,17463,51.8625,E46,S), Array(8,0,3,Palsson,
                                                  //|  Master. Gosta Leonard,male,2,3,1,349909,21.075,,S), Array(9,1,3,Johnson, M
                                                  //| rs. Oscar W (Elisabeth Vilhelmina Berg),female,27,0,2,347742,11.1333,,S), A
                                                  //| rray(10,1,2,Nasser, Mrs. Nicholas (Adele Achem),female,14,1,0,237736,30.070
                                                  //| 8,,C), Array(11,1,3,San
                                                  //| Output exceeds cutoff limit.
  csv(0)                                          //> res0: Array[Any] = Array(PassengerId,Survived,Pclass,Name,Sex,Age,SibSp,Par
                                                  //| ch,Ticket,Fare,Cabin,Embarked)
}