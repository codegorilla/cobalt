package org.cobalt

import java.util.LinkedList

import org.cobalt.symbol.*

// A wrapper object is needed to avoid loss of type information when serializing
// a linked list with Jackson due to Java type erasure.

class Wrapper {

  private var list = LinkedList[Symbol1]()

  def getList (): LinkedList[Symbol1] =
    return list

}
