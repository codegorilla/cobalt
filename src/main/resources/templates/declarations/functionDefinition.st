// Modifiers in C++ are placed in various locations throughout the function
// declaration. We need to handle this properly.

// functionModifiers1: static
// functionModifiers2: virtual
// functionModifiers3: const
// functionModifiers4: final, override

// Access modifiers (e.g. public, private, and protected) need to be handled
// separately (perhaps in an AST transformation phase) because the methods need
// to be aggregated into separate categories when rendered. These modifiers do
// not appear individually on each method.

// Another way to handle access modifiers is to aggregate them under the proper
// section using templates during the code generation phase. This would be done
// similarly to how we have multiple function modifiers functions (see below)
// that each choose their own subset of what to render.

functionDefinition (
  functionModifiers1,
  functionModifiers2,
  functionModifiers3,
  functionModifiers4,
  functionName,
  functionParameters,
  functionReturnType,
  functionBody
) ::= <%
<if(functionModifiers1.functionModifier)><functionModifiers1> <endif>auto<\ >
<if(functionModifiers2.functionModifier)><functionModifiers2> <endif><functionName> <functionParameters><\ >
<if(functionModifiers3.functionModifier)><functionModifiers3> <endif><functionReturnType><\ >
<if(functionModifiers4.functionModifier)><functionModifiers4> <endif><functionBody>
%>
