variableDeclaration (
  variableAccessSpecifier,
  variableModifiers,
  typeSpecifier,
  declarator,
  initializer) ::= <<
<variableAccessSpecifier><\ ><if(variableModifiers.variableModifier)><variableModifiers> <endif><typeSpecifier> <declarator> = <initializer>;
>>
