memberVariableDeclaration (
  variableModifiers,
  typeSpecifier,
  declarator,
  initializer
) ::= <%
<if(variableModifiers)><variableModifiers; separator=" "> <endif>
<typeSpecifier> <declarator> = <initializer>;
%>
