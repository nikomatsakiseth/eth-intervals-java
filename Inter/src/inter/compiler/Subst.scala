package inter.compiler

class Subst {
    def ty(t: Symbol.Type): Symbol.Type
}

object Subst {
    def expr(state: CompilationState, syms: List[Symbol.Var], exprs: List[Hl.CT.Expr]): Subst = {
        null
    }
}