package org.cobalt

import scala.collection.mutable.Map


import java.util.LinkedList
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

@main def hello () =

  // We want to create a module compiler that can act independently (possibly
  // in parallel with other module compiler instances) to compile a single
  // module. Note that the main function exists outside of any module.

  val translator = Translator()
  translator.process()
