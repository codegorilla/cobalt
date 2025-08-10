// Modifiers in C++ are placed in various locations throughout the function
// declaration. We need to handle this properly.

// functionModifiers3: const

memberFunctionDefinition (
  functionModifiers3,
  functionName,
  functionParameters,
  functionReturnType,
  functionBody
) ::= <%
auto <functionName> <functionParameters><\ >
<if(functionModifiers3.functionModifier)><functionModifiers3> <endif><functionReturnType><\ >
<functionBody>
%>
