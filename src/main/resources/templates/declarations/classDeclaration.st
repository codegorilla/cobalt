classDeclaration (
  classAccessSpecifier,
  classModifiers,
  className,
  classBody
) ::= <%
<if(classAccessSpecifier)><classAccessSpecifier> <endif>
class <className><\ >
<if(classModifiers)><classModifiers; separator=" "> <endif>
<classBody>;
%>
