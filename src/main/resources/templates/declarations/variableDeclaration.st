variableDeclaration (
  variableAccessSpecifier,
  variableModifiers,
  typeSpecifier,
  declarator,
  initializer
) ::= <%
<if(variableAccessSpecifier)><variableAccessSpecifier> <endif>
<if(variableModifiers.variableModifier)><variableModifiers> <endif>
<typeSpecifier> <declarator> = <initializer>;
%>
