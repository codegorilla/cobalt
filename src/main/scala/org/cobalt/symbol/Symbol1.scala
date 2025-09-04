package org.cobalt.symbol

import com.fasterxml.jackson.annotation.JsonTypeInfo

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "cat")
class Symbol1 (name: String) {

  def getName (): String =
    return name

}
