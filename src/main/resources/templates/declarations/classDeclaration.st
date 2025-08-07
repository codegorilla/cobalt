classDeclaration (
  classModifiers1,
  classModifiers2,
  className,
  classBody
) ::= <%
<if(classModifiers1.classModifier)><classModifiers1> <endif>class <className><\ >
<if(classModifiers2.classModifier)><classModifiers2> <endif>
<classBody>
%>
