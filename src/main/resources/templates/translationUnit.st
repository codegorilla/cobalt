translationUnit (packageDeclaration, declaration) ::=  <<
<packageDeclaration>

import std;

namespace <packageDeclaration.name> {<\n>
  <declaration; separator="\n\n">

}
>>
