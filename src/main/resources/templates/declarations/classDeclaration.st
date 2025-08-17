classDeclaration (
  classAccessSpecifier,
  classModifiers,
  className,
  baseClause,
  classBody
) ::= <%
<if(classAccessSpecifier)><classAccessSpecifier> <endif>
class <className><\ >
<baseClause>
<if(classModifiers)><classModifiers; separator=" "> <endif>
<classBody>;
%>
