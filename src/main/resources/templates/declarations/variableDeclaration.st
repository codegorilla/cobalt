variableDeclaration (
  variableAccessSpecifier,
  variableModifiers,
  typeSpecifier,
  declarator,
  initializer
) ::= <%
<if(variableAccessSpecifier)><variableAccessSpecifier> <endif>
<if(variableModifiers)><variableModifiers; separator=" "> <endif>
<typeSpecifier> <declarator> = <initializer>;
%>
