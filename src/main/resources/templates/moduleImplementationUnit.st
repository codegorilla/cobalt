// We will probably set some_module to the name of the file or directory.
// If not all elements are definitions, we can use element, item, or some other
// moniker instead.

// Not used?

moduleImplementationUnit (definition) ::=  <<
module some_module;
import std;
<definition; separator="\n">
>>
