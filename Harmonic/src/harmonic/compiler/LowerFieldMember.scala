package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Ast.Lower.Extensions._
import Util._

class LowerFieldMember(
    global: Global,
    csym: ClassFromSource,
    inFieldDecl: in.FieldDecl
) extends LowerMember(global, csym, inFieldDecl) {
    
    // ___ Symbol Interval __________________________________________________
    
    private[this] val symInter: Interval = {
        def createFieldSymbol(ty: Type.Ref) = {
            new VarSymbol.Field(
                pos       = inFieldDecl.pos,
                modifiers = Modifier.forResolvedAnnotations(inFieldDecl.annotations),
                name      = inFieldDecl.name,
                ty        = ty,
                kind      = FieldKind.Harmonic
            )            
        }
        
        global.master.subinterval(during = List(csym.members)) { inter =>
            sym.v = inFieldDecl.tref match {
                case tref: in.ResolvedTypeRef => {
                    createFieldSymbol(Lower(global).InEnv(csym.classEnv).lowerTypeRef(tref))
                }

                case in.InferredTypeRef() => {
                    try {
                        createFieldSymbol(outFieldDecl.join.tref.ty)
                    } catch {
                        case _: IntervalException.cycle => {
                            Error.CircularMemberType(inFieldDecl.name).report(
                                global, inFieldDecl.pos
                            )
                            createFieldSymbol(Type.Object)
                        }
                    }
                }
            }                    
            
        }
    }
    private[this] val sym = new GuardedBy[VarSymbol.Field](symInter)
    
    override def toOptFieldSymbol(memName: Name.Member): Option[VarSymbol.Field] = {
        if(inFieldDecl.isNamed(memName)) {
            try {
                Some(sym.join)                
            } catch {
                case _: IntervalException.cycle => {
                    Error.CircularMemberType(inFieldDecl.name).report(
                        global, inFieldDecl.pos
                    )
                    VarSymbol.errorField(memName, None)
                }
            }
        } else None
    }
    
    // ___ Body Interval ____________________________________________________

    private[this] val bodyInter: Interval = {
        global.master.subinterval(during = List(csym.members)) { inter =>
            debugIndent("lowering %s", inFieldDecl) {
                outFieldDecl.v = Lower(global).lowerFieldDecl(csym, inFieldDecl)                
            }
        }
    }
    
    private[this] val outFieldDecl = new GuardedBy[out.FieldDecl](bodyInter)

    override def memberDecl: out.MemberDecl = outFieldDecl.v
    
}