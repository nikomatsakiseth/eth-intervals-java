package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

abstract class LowerMember(
    global: Global,
    csym: ClassFromSource,
    val inMemberDecl: in.MemberDecl
) {
    
    /** Read lowered member decl without blocking */
    def memberDecl: out.MemberDecl

    /** If inMemberDecl is a method named `mthdName`, returns the method symbol. */
    def toOptMethodSymbol(mthdName: Name.Method): Option[MethodSymbol] = None
    
    /** If inMemberDecl is a field named `memName`, returns the field symbol. */
    def toOptFieldSymbol(memName: Name.Member): Option[VarSymbol.Field] = None
    
}