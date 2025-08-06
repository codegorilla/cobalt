package org.cobalt

class ErrorMessage (val line: Int, val column: Int) {

  var message: String = null

  def setMessage (message: String) = {
    this.message = message
  }

  def print () = {
    println(s"(${line}, ${column}): " + message)
  }
}
