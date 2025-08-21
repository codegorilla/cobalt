package org.cobalt

import java.nio.file.Path
import java.nio.file.Files
import java.io.IOException

class Reader {
  
  var input: Path = null

  def setInput (input: Path) =
    this.input = input

  def process (): String =
    var content: String = null
    try
      content = Files.readString(input)
    catch
      case e: IOException => e.printStackTrace()
    return content

}
