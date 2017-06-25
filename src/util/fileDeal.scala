package util

import java.io.{ File, FileInputStream, FileOutputStream, InputStream, OutputStream, IOException }
import java.util.zip.{ ZipFile, ZipEntry, ZipInputStream }

import scala.collection.immutable.{ Map => immuMap }
import scala.collection.mutable.ArrayBuffer
import scala.xml.{ XML, Node => xmlNode }
import scala.io.Source
import scala.annotation.tailrec

object fileDeal {
  val pass = null /* something like keywords which might look graceful */

  object xlsx {

    /*Generate the dataframe source.*/
    def xmlParser(source: File, shareStringFile: File): Array[Array[Any]] = {

      val shareStrings: Array[String] = ((XML.loadFile(shareStringFile) \ "si") \ "t").
        map(_.text).toArray

      def mapping(elem: xmlNode): Any = {
        if (elem.attributes("t") == null) {
          elem.text.toDouble
        } else {
          shareStrings(augmentString(elem.text).toInt)
        }
      }

      ((XML.loadFile(source) \ "sheetData") \ "row").
        map { (row) => (row \ "c").map(mapping(_)).toArray }.toArray

    }

    def unzipXlsxFile(input: String, output: String, dictArgs: immuMap[String, Any] = immuMap()): Unit = {
      /*unzip a xlsx file into a directory */

      //deal with default arguments
      val filter = if (dictArgs.isDefinedAt("filter")) dictArgs("filter").asInstanceOf[String => Boolean] else null
      val verbose = if (dictArgs.isDefinedAt("verbose")) dictArgs("verbose").asInstanceOf[Boolean] else false

      //some initial arguments
      val outputFix: String = if (output.endsWith("/")) output else output + "/" /* deal with the output directory */
      var zipInput: ZipInputStream = new ZipInputStream(
        new FileInputStream(
          new File(input)))
      var zipFile: ZipFile = new ZipFile(input)

      //iterator formed as cycle.
      while (null != /* if the input-stream is not empty, do works. */
        ((zipin: ZipInputStream) => {
          var entry: ZipEntry = zipin.getNextEntry()
          if (entry == pass) { pass } /* if the  */
          else {

            if (verbose) println(entry.getName)

            ((file: File) => {

              if (filter != null && !filter(file.getAbsolutePath())) { pass }

              else if ((if (!file.getParentFile().exists()) { file.getParentFile().mkdirs(); true } else false)
                ||
                (!file.exists()) /*for optimization*/ ) {
                try {
                  /* some item in the zipped file doesn't exist really...
                    	* so use try to ignore
                    	*/
                  file.createNewFile();
                  var input: InputStream = zipFile.getInputStream(entry)
                  var fileOut: FileOutputStream = new FileOutputStream(file)
                  while (((readInstream: InputStream) => { val readin = readInstream.read(); if (readin != (-1)) { fileOut.write(readin); true } else false })(input)) { /*pass*/ }
                  fileOut.close()
                  input.close()
                } catch {
                  case e: IOException => { pass }
                  /* if this item doesn't exists, ignore it.*/
                  case _: Throwable   => throws
                }
              }
              pass
            })(new File(outputFix + entry.getName()))
            entry
          }
        })(zipInput)) { /*pass*/ }

      zipFile.close()
      zipInput.close()
    }

    def read(path: String): Array[Array[Array[Any]]] = {

      def parseFile(files: Array[File], shareStringFile: File): Array[Array[Array[Any]]] = {
        refArrayOps(files).map(xmlParser(_, shareStringFile))
      }

      val pathStructure: Array[String] = path.split("/")
      val parentDirectory = refArrayOps(refArrayOps(pathStructure).slice(1, pathStructure.length - 1)).toList.foldLeft(pathStructure(0))(_ + "/" + _)
      var tempPath = refArrayOps(pathStructure).last.replace(".xlsx", "$")
      var CurrentDir = refArrayOps(refArrayOps(new File(parentDirectory).listFiles()).map(_.getName())).toSet

      while (CurrentDir.contains(tempPath)) tempPath += "$" /*geneate a file whose name is not reduplicated*/

      tempPath = parentDirectory + "/" + tempPath
      unzipXlsxFile(path, tempPath, immuMap("filter" -> //filter the file whose name does not contain any item in ["xl\worksheets" , "xl\shareString.xml"] 
        ((x: String) => x.contains("xl\\worksheets") || x.contains("xl\\sharedStrings.xml"))))

      val sharedString = tempPath + "/xl/sharedStrings.xml"
      val sourcePath = tempPath + "/xl/worksheets/"
      val taskFile: File = new File(sourcePath)

      val ret = parseFile(refArrayOps(taskFile.listFiles()).filter((x: File) => x.getName().endsWith(".xml")), new File(sharedString))
      dirDeal.delete(new File(tempPath)) //delete the unzipped files.
      ret
    }

  }
  object csv {

    def read(path: String, sep: Char = ','): Array[Array[Any]] = {
      val buffer = ArrayBuffer[ArrayBuffer[Any]]()
      val bufferString = Source.fromFile(path)

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
    }
  }

}