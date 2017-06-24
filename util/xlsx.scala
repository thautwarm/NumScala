
package util
import java.io.{File,FileInputStream,FileOutputStream,InputStream,OutputStream,IOException};
import java.util.zip.{ZipFile,ZipEntry,ZipInputStream}
import scala.collection.immutable.{Map=>immuMap}
import scala.collection.mutable.{Set=>muSet,Seq=>muSeq,Stack}
import scala.xml.{XML,Node=>xmlNode}
import scala.io.Source
import util.dirDeal
object xmlParser {
    /*Generate the dataframe source.*/
    def getData(source:File,shareStringFile:File):Array[Array[String]]={
           
          val shareStrings:Array[String] =((XML.loadFile(shareStringFile) \ "si") \ "t").
             map(_.text).toArray
             
          def mapping(elem:xmlNode):String={
               if (elem.attributes("t")==null){
                  elem.text
                }
               else{
                 shareStrings(augmentString(elem.text).toInt)
                }
              }
          
          ((XML.loadFile(source) \ "sheetData" ) \ "row").
            map{ (row)=>(row \ "c").map(mapping(_)) .toArray }.toArray
        
     }
}
object xlsx {
    def unzipXlsxFile(input:String,output:String,dictArgs:immuMap[String,Any]=immuMap()):Unit={
        /*unzip a xlsx file into a directory */

        //deal with default arguments
        val filter =  if(dictArgs.isDefinedAt("filter"))  dictArgs("filter").asInstanceOf[String=>Boolean] else null
        val verbose = if(dictArgs.isDefinedAt("verbose")) dictArgs("verbose").asInstanceOf[Boolean] else false

        //some initial arguments
        val outputFix:String =  if (output.endsWith("/")) output else output+"/" /* deal with the output directory */
        val pass=null  /* something like keywords which might look graceful */
        var zipInput:ZipInputStream =new ZipInputStream(
                                  new FileInputStream(
                                  new File(input)    ))
        var zipFile:ZipFile = new ZipFile(input)


        //iterator formed as cycle.
        while(null !=  /* if the input-stream is not empty, do works. */
         ((zipin:ZipInputStream)=>{
            var entry: ZipEntry = zipin.getNextEntry()
            if (entry==pass) {pass} /* if the  */
            else{
              
              if (verbose) println(entry.getName) 
              
              ((file:File)=> {
                
                if (filter!=null && !filter(file.getAbsolutePath()) ) {pass}
              
                else if  (
                     ( if (! file.getParentFile().exists() ){file.getParentFile().mkdirs();true} else false)
                     ||
                     (!file.exists())                       /*for optimization*/
                         )
                    {
                     try{
                     /* some item in the zipped file doesn't exist really...
                      * so use try to ignore
                      */
                     file.createNewFile();
                     var input:InputStream = zipFile.getInputStream(entry)
                     var fileOut:FileOutputStream=new FileOutputStream(file)
                     while( ((readInstream:InputStream)=>{val readin=readInstream.read(); if (readin != (-1) ){ fileOut.write(readin);true} else false} ) (input)
                        ) {/*pass*/}
                     fileOut.close()
                     input.close()
                      }
                    catch
                      {
                       case e:IOException=>{pass}
                          /* if this item doesn't exists, ignore it.*/
                       case _:Throwable=> throws
                       }
                   }
                pass
                })(new File (outputFix+entry.getName()))
             entry
            }
            })(zipInput)
            ){/*pass*/}

        zipFile.close()
        zipInput.close()
    }

    def parseXlsx(path:String)={

      val pathStructure:Array[String] = path.split("/")
      val parentDirectory = refArrayOps(refArrayOps(pathStructure).slice(1, pathStructure.length-1)).toList.foldLeft(pathStructure(0))(_+"/"+_)
      var tempPath= refArrayOps(pathStructure).last.replace(".xlsx", "$")
      var CurrentDir = refArrayOps(refArrayOps(new File(parentDirectory).listFiles()).map(_.getName())).toSet

      while (CurrentDir.contains(tempPath)) tempPath+="$"  /*geneate a file whose name is not reduplicated*/

      tempPath = parentDirectory+"/"+tempPath
      
      unzipXlsxFile(path,tempPath,immuMap( "filter"-> //filter the file whose name does not contain any item in ["xl\worksheets" , "xl\shareString.xml"] 
                   ((x:String)=> x.contains("xl\\worksheets")|| x.contains("xl\\sharedStrings.xml") )))
      
      val sharedString =tempPath + "/xl/sharedStrings.xml"
      val sourcePath=tempPath+"/xl/worksheets/"
      val taskFile:File = new File(sourcePath)

      val ret = parseXmlFile (refArrayOps(taskFile.listFiles()).filter( (x:File)=> x.getName().endsWith(".xml")), new File(sharedString) )
      dirDeal.delete(new File(tempPath))
      ret
    }

    def parseXmlFile(files:Array[File],shareStringFile:File):Array[Array[Array[String]]]={
      refArrayOps(files).map(xmlParser.getData(_,shareStringFile))
    }
    def main(args:Array[String]):Unit={  //Test
      var df =parseXlsx("F:/Supports/1.xlsx").map(_.map(_.toList).toList).toList
      println(df)
//      unzipXlsxFile("C:/Users/thautwarm/Desktop/demo/mas.xlsx","C:/Users/thautwarm/Desktop/demo/mas_unzip",immuMap("filter"->((x:String)=> x.contains("xl\\worksheets"))))
      }


}
    
