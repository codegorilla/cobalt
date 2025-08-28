package org.cobalt

@main def hello () =

  // Many CLI parsers use annotations or other DSL techniques that make them
  // hard to apply outside of their native implementation language. I don't want
  // to spend a lot of time on this since the goal is to re-write the code in
  // cobalt itself, where such libraries won't be available. Thus, for now, we
  // can simulate processing of CLI parameters or just do a rudimentary job that
  // is just enough to get by.

  // Simulate "co build .", which says to build the main package and all of its
  // dependencies. It will automatically find all files in the current
  // directory.

  // We want to create a package compiler that can act independently (possibly
  // in parallel with other package compiler instances) to compile a single
  // package. Note that the main function exists outside of any package.

  val translator = Translator()
  translator.process()
