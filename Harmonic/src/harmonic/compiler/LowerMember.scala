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
    /// Lowered member decl, only accessible after the `lower` interval
    /// for the class is completed.
    def memberDecl: out.MemberDecl
    
    /// Returns `Some(sym)` if this is a method named `mthdName`.
    /// Joins the interval(s) creating the symbol as needed.
    def toOptMethodSymbol(mthdName: Name.Method): Option[MethodSymbol] = None

    /// Like `toOptMethodSymbol()`, but for fields.
    /// Joins the interval(s) creating the symbol as needed.
    def toOptFieldSymbol(memName: Name.Member): Option[VarSymbol.Field] = None
}