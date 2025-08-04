// Modifiers in C++ are placed in various locations throughout the function
// declaration. We need to handle this properly.

// functionModifiers1: static
// functionModifiers2: virtual
// functionModifiers3: const
// functionModifiers4: final, override

functionDeclaration (
  functionModifiers1,
  functionModifiers2,
  functionModifiers3,
  functionModifiers4,
  functionName,
  functionParameters,
  functionReturnType,
  functionBody
) ::= <%
<if(functionModifiers1.functionModifier)><functionModifiers1> <endif>auto<\ >
<if(functionModifiers2.functionModifier)><functionModifiers2> <endif><functionName> <functionParameters><\ >
<if(functionModifiers3.functionModifier)><functionModifiers3> <endif><functionReturnType><\ >
<if(functionModifiers4.functionModifier)><functionModifiers4> <endif><functionBody>
%>
