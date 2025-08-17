// Modifiers in C++ are placed in various locations throughout the function
// declaration. We need to handle this properly.

// cvQualifiers: const, volatile

memberFunctionDefinition (
  cvQualifiers,
  functionName,
  functionParameters,
  functionReturnType,
  functionBody
) ::= <%
auto <functionName> <functionParameters><\ >
<if(cvQualifiers.cvQualifier)><cvQualifiers> <endif><functionReturnType><\ >
<functionBody>
%>
