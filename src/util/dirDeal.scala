package util
import java.io.File
import java.nio.file.Path
object dirDeal {
  def delete(file:File):Unit={
    if (file.isDirectory()) {file.listFiles().map(delete(_));file.delete()} else file.delete()
  }
  
  
  
}