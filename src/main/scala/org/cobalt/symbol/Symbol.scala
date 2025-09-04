package org.cobalt.symbol

import com.fasterxml.jackson.annotation.JsonTypeInfo

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "cat")
abstract class Symbol (name: String) {

  def getName (): String =
    return name

}

// Don't forget about the following:
// CLASS_TEMPLATE
// METHOD
// METHOD_TEMPLATE
// MODULE
// PACKAGE
// ROUTINE_TEMPLATE
