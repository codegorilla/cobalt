// Modifiers in C++ are placed in various locations throughout the function
// declaration:
// functionModifiers1: constexpr
// functionModifiers2: noexcept (not yet implemented)

functionDeclaration (
  functionAccessSpecifier,
  functionModifiers1,
  functionModifiers2,
  functionName,
  functionParameters,
  functionReturnType
) ::= <%
<if(functionAccessSpecifier)><functionAccessSpecifier> <endif>
<if(functionModifiers1)><functionModifiers1; separator=" "> <endif>
auto <functionName> <functionParameters> <functionReturnType>;
%>
