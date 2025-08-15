// Modifiers in C++ are placed in various locations throughout the member
// function declaration:
// memberFunctionModifiers1: constexpr, static, virtual
// memberFunctionModifiers2: const, volatile (not yet implemented)
// memberFunctionModifiers3: &, && (not yet implemented)
// memberFunctionModifiers4: noexcept, final, override (not yet implemented)

// To do: Member function access specifiers use group heading syntax.

// Note: memberFunctionModifiers2 are also called cv-qualifiers and
// memberFunctionModifiers3 are also called ref-qualifiers.

memberFunctionDeclaration (
  memberFunctionAccessSpecifier,
  memberFunctionModifiers1,
  memberFunctionModifiers2,
  memberFunctionModifiers3,
  memberFunctionModifiers4,
  memberFunctionName,
  memberFunctionParameters,
  memberFunctionReturnType
) ::= <%
<if(memberFunctionAccessSpecifier)><memberFunctionAccessSpecifier> <endif>
<if(memberFunctionModifiers1)><memberFunctionModifiers1; separator=" "> <endif>
auto <memberFunctionName> <memberFunctionParameters><\ >
<if(memberFunctionModifiers2)><memberFunctionModifiers2; separator=" "> <endif>
<if(memberFunctionModifiers3)><memberFunctionModifiers3; separator=" "> <endif>
<memberFunctionReturnType>
<if(memberFunctionModifiers4)><memberFunctionModifiers4; separator=" "> <endif>;
%>
