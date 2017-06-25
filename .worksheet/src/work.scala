import scala.collection.immutable.{Seq=>imSeq,Vector=>imVec,Queue=>imQ}
import scala.collection.mutable.{Seq=>mSeq,ArraySeq,Queue=>mQ,ListBuffer,LinearSeq,ArrayBuffer}
import scala.compat.Platform.currentTime
import java.io.{ File, FileInputStream, FileOutputStream, InputStream, OutputStream, IOException }
import scala.io.Source
import scala.annotation.tailrec
object work {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(393); 
	val pass = null;System.out.println("""pass  : Null = """ + $show(pass ));$skip(1945); 
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

  };System.out.println("""TextParser: (source: String, sep: Char)Array[Array[Any]]""");$skip(60); 
  val csv=TextParser("H:/Kaggle/TitanicDisaster/train.csv");System.out.println("""csv  : Array[Array[Any]] = """ + $show(csv ));$skip(9); val res$0 = 
  csv(0);System.out.println("""res0: Array[Any] = """ + $show(res$0))}
}
