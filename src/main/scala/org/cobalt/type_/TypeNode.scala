package org.cobalt.type_

import com.fasterxml.jackson.annotation.JsonTypeInfo

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "cat")
abstract class TypeNode {}
