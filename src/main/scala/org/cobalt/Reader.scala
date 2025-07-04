package org.cobalt

import java.io.BufferedReader
import java.io.FileReader

class Reader {
  
  var reader: BufferedReader = null

  def setInput (filename: String) = {
    reader = BufferedReader(FileReader(filename))
  }

  def process () = {
    
  }

}
