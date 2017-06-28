package com.thautwarm.numscala.util

import java.io.File

object dirDeal {
  def delete(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().map(delete);
      file.delete()
    } else file.delete()
  }


}