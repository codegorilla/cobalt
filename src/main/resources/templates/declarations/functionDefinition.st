// Modifiers in C++ are placed in various locations throughout the function
// declaration. We need to handle this properly.

// Inline function definitions only appear in the module implementation unit if
// it will NOT be exported. If it will be exported, then the definition must
// appears with the declaration in the module interface unit.

// functionModifiers3 will be renamed to type qualifier, and may be one of
// 'const', '&', or '&&'. For now just handle const.

// functionModifiers1: inline
// functionModifiers3: const

functionDefinition (
  functionModifiers1,
  functionModifiers3,
  functionName,
  functionParameters,
  functionReturnType,
  functionBody
) ::= <%
<if(functionModifiers1.functionModifier)><functionModifiers1> <endif>auto<\ >
<functionName> <functionParameters><\ >
<if(functionModifiers3.functionModifier)><functionModifiers3> <endif><functionReturnType><\ >
<functionBody>
%>
