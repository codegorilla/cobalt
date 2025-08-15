// Modifiers in C++ are placed in various locations throughout the function
// declaration. We need to handle this properly.

// functionModifiers1: constexpr
// functionModifiers2: noexcept (not yet implemented)

functionDeclaration (
  functionAccessSpecifier,
  functionModifiers1,
  functionName,
  functionParameters,
  functionReturnType
) ::= <%
<if(functionAccessSpecifier)><functionAccessSpecifier> <endif>
<if(functionModifiers1)><functionModifiers1; separator=" "> <endif>
auto <functionName> <functionParameters> <functionReturnType>;
%>
