// ___ Rules ____________________________________________________________

// ______ Path and Type facts ___________________________________________
//
// These are kind of a workaround that allows us to avoid using backwards
// propagation and its inherent complications with transitive relations.
//
// Basically we add a Path or Type fact for every referenced type.  
// This is then used to trigger various forward rules that handle 
// "0-predicate" rules.

class PathFactRule(
    val kind: Class[_ <: Fact.Paths]
) extends Rule.Forward {
    val inputKinds = List(kind)
    
    def derive(state: Network#State, facts: List[Fact]) = {
        val List(Fact.Paths(l, r)) = facts
        List(Fact.Path(l), Fact.Path(r))
    }
}
addRule(new PathArgRule(classOf[Fact.Eq]))
addRule(new PathArgRule(classOf[Fact.Hb]))
addRule(new PathArgRule(classOf[Fact.SubOf]))
addRule(new PathArgRule(classOf[Fact.InlineSubOf])) // etc

class TypeFactRule(
    val kind: Class[_ <: Fact.Types]
) extends Rule.Forward {
    val inputKinds = List(kind)
    
    def derive(state: Network#State, facts: List[Fact]) = {
        val List(Fact.Types(l, r)) = facts
        List(Fact.Type(l), Fact.Type(r))
    }
}
addRule(new TypeArgRule(classOf[Fact.TypeEq]))
addRule(new TypeArgRule(classOf[Fact.TypeSub]))
addRule(new TypeArgRule(classOf[Fact.TypeSup]))

// ______ Type Arguments ________________________________________________

/*
p : c[..., f rel q, ...]
--------------------
p.(c.f) rel q
*/
addRule(new Rule.ReflectiveForward {
    def factsFor(path: Path.Ref): List[Path.Ref] = path match {
        case Path.Field(p, f) => {
            val baseFacts = factsFor(p)
            state.upcast(p, c).foldRight(baseFacts) {
                case (Type.Class(_, args), facts) => {
                    args.mapAsPrefix(facts) {
                        case Type.PathArg(f, rel, q) => rel(Path.Field(p, f), q)
                        case Type.TypeArg(f, rel, t) => rel(Type.Member(p, f), t)
                    }
                }
            }
        }
        
        case Path.Cast(t, base) => factsFor(base)
        case Path.Index(array, index) => factsFor(array) ++ factsFor(index)
        case _ => // etc
    }
    
    def trigger(state: Network#State, fact: Fact.Path) = {
        factsFor(fact.path)
    }
})

// ______ Equivalence Propagation _______________________________________
//
// The `Eq` relation has the nice property that any two paths that are
// equivalent are also equivalent with respect to all other relations.
// The following three templates implement this rule.  

class PathEqualityRule[F <: Fact](
    val otherKind: Class[F]
    ctor: ((Path, Path) => F)
) extends Rule.Forward {
    val inputKinds = List(classOf[Fact.Eq], otherKind)
    
    def trigger(state: Network#State, facts: eqFact: Fact.Eq, otherFact: Fact) = {
        val List(Fact.Eq(p, q), Fact.Paths(l, r)) = otherFact
        List(
            p.is(l).toOption(Fact.Paths(q, r)),
            p.is(r).toOption(Fact.Paths(l, q)),
            q.is(l).toOption(Fact.Paths(p, r)),
            q.is(r).toOption(Fact.Paths(l, p))
        ).flatten
    }
}

addRule(new PathEqualityRule(classOf[Fact.Eq], Fact.Hb))
addRule(new PathEqualityRule(classOf[Fact.Hb], Fact.Hb))
addRule(new PathEqualityRule(classOf[Fact.SubOf], Fact.Hb))
addRule(new PathEqualityRule(classOf[Fact.InlineSubOf], Fact.Hb))
// etc

// ______ Path Equality _________________________________________________

/*
--------------------
p == p
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "PathEq-Reflexive"
    
    def trigger(state: Network#State, eq: Fact.Path) = {
        val Fact.Path(p) = eq
        Some(Fact.PathEq(p, p))
    }
})

/*
p == q
--------------------
q == p
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "PathEq-Symmetric"
    
    def trigger(state: Network#State, eq: Fact.PathEq) = {
        val Fact.PathEq(p, q) = eq
        Some(Fact.PathEq(q, p))
    }
})

/*
--------------------
(T)p == p
etc

Also handles the reflexive case (p == p).
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Path-Simplify"
    
    def trigger(state: Network#State, fact: Fact.Path) = {
        val Fact.Path(path) = fact
        fact match {
            case Path.Cast(_, base) => Some(Fact.PathEq(path, base))
            case Path.Tuple(List(base)) => Some(Fact.PathEq(path, base))
            case Path.Index(
                Path.Tuple(paths), 
                Path.Constant(index: java.lang.Integer)
            ) if index.intValue < paths.length => Some(Fact.PathEq(path, paths(index.intValue)))
            case _ => None
        }
    }
})

/*
p == q
--------------------
p.f == q.f
etc
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Path-Induction-Path-Eq)"
    
    def trigger(state: Network#State, fact: Fact.Path, eq: Fact.PathEq) = {
        val (Fact.Path(path), Fact.PathEq(p, q)) = (fact, eq)
        Some(Fact.PathEq(path, Subst(p -> q).path(path)))
    }
})

// ______ Type Equality _________________________________________________

/*
t1 == t2
--------------------
t2 == t1
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Type-Eq-Reflexive"
    
    def trigger(state: Network#State, eq: Fact.TypeEq) = {
        val Fact.TypeEq(ty1, ty2) = eq
        Some(Fact.TypeEq(ty2, ty1))
    }
})

/*
--------------------
(t) == t
etc

Also handles the reflexive case (t == t).
*/
add(new Template.ReflectiveForward() {
    def trigger(state: Network#State, fact: Fact.Type) = {
        val Fact.Type(ty) = fact
        ty.matchAll(
            { case _ => Fact.PathEq(ty, ty) },
            { case Type.Tuple(List(ty1)) => Fact.PathEq(ty, ty1) },
            { case Type.Tuple(Nil) => Fact.PathEq(ty, Type.Void) }
        )
    }
})

/*
p == q
--------------------
p.X == q.X
where p/q are paths, X is a type variable
*/
add(new Template.ReflectiveForward() {
    override def toString = "Type-Induction-Path-Eq"
    
    def trigger(state: Network#State, fact: Fact.Type, factEq: Fact.PathEq) = {
        val Fact.PathEq(p, q) = factEq
        val Fact.Type(ty) = fact
        Some(Fact.TypeEq(ty, Subst(p -> q).ty(ty)))
    }
})

// ______ Sub- and super-typing _________________________________________

/*
class c1 extends c2
--------------------
c1[args] <: c2[args]
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Extends"
    
    def trigger(state: Network#State, sub: Fact.Type) = {
        sub.ty.matchAll(
            { case t1 @ Type.Class(nm, args) => 
                val csym = state.global.csym(nm)
                csym.superClassNames.map { c2 =>
                    // TODO: Add in any applicable constraints defined in c1!!
                    // TODO: Filter out inapplicable arguments from ty (won't hurt though)
                    Fact.TypeSub(t1, Type.Class(t1, args))
                }
            }
        ).flatten
    }
})

/*
t1 <: t2
--------------------
t2 :> t1
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Sub-Implies-Sup"
    
    def trigger(state: Network#State, sub: Fact.TypeSub) = {
        val Fact.TypeSub(t1, t2) = sub1
        Some(Fact.TypeSup(t2, t1))
    }
})

/*
t1 :> t2
--------------------
t2 <: t1
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Sup-Implies-Sub"
    
    def trigger(state: Network#State, sub: Fact.TypeSup) = {
        val Fact.TypeSup(t1, t2) = sub1
        Some(Fact.TypeSub(t2, t1))
    }
})

/*
t1 <: t2
t2 <: t1
--------------------
t1 = t2
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Sub-Sup-Imply-Eq"
    
    def trigger(state: Network#State, sub: Fact.TypeSub, sup: Fact.TypeSup) = {
        val Fact.TypeSub(t1, t2) = sub1
        val Fact.TypeSup(t3, t4) = sub1
        (t1 == t4 && t3 == t2) toOption Fact.TypeEq(t1, t2)
    }
})

/*
t1 <: t2
t2 <: t3
--------------------
t1 <: t3
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Sub-Transitive"
    
    def trigger(state: Network#State, sub1: Fact.TypeSub, sub2: Fact.TypeSub) = {
        val Fact.TypeSub(t1, t2) = sub1
        val Fact.TypeSub(t3, t4) = sub2
        (t2 == t3).toOption(Fact.TypeSub(t1, t4))
    }
})

/*
t1 == t2
--------------------
t1 <: t2
t1 :> t2
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "Eq-Implies-Sub-Sup"
    
    def trigger(state: Network#State, fact: Fact.TypeEq) = {
        val Fact.TypeEq(t1, t2) = fact
        List(Fact.TypeSub(t1, t2), Fact.TypeSup(t1, t2))
    }
})

/*
args1 => args2
--------------------
c[args1] <: c[args2]
*/
addRule(new Rule.ReflectiveBackward() {
    def trigger(state: Network#State, fact: Fact.TypeSub) = {
        fact match {
            case Fact.TypeSub(Type.Class(c1, args1), Type.Class(c2, args2)) if (c1 == c2) => {
                // To determine if args1 => args2:
                // * Gin up a fake local variable x of type c[args1].
                // * Check whether x → c[args2].
                
                // TODO

                val fresh = new VarSymbol.Local(
                    pos = NoPosition,
                    modifiers = Modifier.Set.empty,
                    name = state.global.freshLocalName,
                    ty = Type.Class(c, args1)
                )

                val state2 = state.copy(
                    locals = state.locals + (fresh.name -> fresh)
                )
                
                state2.contains(Fact.Assignable(fresh.toPath, Type.Class(c, arg2)))
            }
            case _ => false
        }
    }
})

// ______ Assignable ____________________________________________________
// 
// Important: You might think that Assignable would be implemented in 
// terms of a TRT query like (path.ty <: ty), but this is not the case!
// In fact, it is the opposite: TRT subtyping queries are implemented
// in terms of assignability.
//
// This is because we may know information about the path being assigned 
// from that is not encoded in the path's type.  In that case, these
// extra facts may be useful to showing that path is assignable to ty.

addRule(new Rule.ReflectiveBackward() {
// XXX

add(new RuleTemplate() {
    case class Assignable(path: Path.Ref, ty: Type) extends Rule {
        override def deriveFacts(state: ProofState) = {
            def isSatisfiedForPath(arg: Type.Arg): Boolean = {
                arg match {
                    case Type.PathArg(name, rel, path2) => {
                        val extPath = Path.Field(path, name)
                        state.rel(extPath, rel, path2)
                    }
                    case Type.TypeArg(name, rel, ty) => {
                        val extTy = Type.Member(path, name)
                        state.rel(extTy, rel, ty)
                    }
                }            
            }
            
            val ubPathTys = state.upperBounds(state.typeOfPath(path))
            val lbLvalueTys = state.lowerBounds(ty)
            val isAssignable = (ubPathTys cross lbLvalueTys).exists {
                case (Type.Null, _) =>
                    true

                case (Type.Class(n1, _), Type.Class(n2, args)) if n1.is(n2) =>
                    args.forall(isSatisfiedForPath)

                case (t1, t2) =>
                    t1.is(t2)
            }
            
            isAssignable.toOption(Fact.Assignable(path, ty))
        }
    }
    
    def instantiate(state: ProofState)(query: Query[_]) = query match {
        case Query.Assignable(path, ty) => Some(Assignable(path, ty))
        case _ => None
    }
})

// ______ subOf, inlineSubOf, hb ________________________________________

/*
pc inlineSubOf pp
--------------------
pc subOf pp
*/

addRule(new Rule.ReflectiveForward() {
    override def toString = "InlineSubOf-Implies-SubOf"
    
    def trigger(state: Network#State, inlineSubOf: Fact.InlineSubOf) = {
        val Fact.InlineSubOf(pc, pp) = inlineSubOf
        Some(Fact.SubOf(pc, pp))
    }
})

/*
pc subOf pp
------------
pp.start hb pc.start
pc.end hb pp.end
*/
addRule(new Rule.ReflectiveForward() {
    override def toString = "SubOf-Implies-HB"
    
    def trigger(state: Network#State, subOf: Fact.SubOf) = {
        import MethodId.{GetStart, GetEnd}
        val Fact.SubOf(pc, pp) = subOf
        List(
            Fact.Hb(pp.call(GetStart), pc.call(GetStart)),
            Fact.Hb(pc.call(GetEnd), pp.call(GetEnd))
        )
    }
})

// ______ permitsWr, permitsRd, ensuresFinal ____________________________

/*
g permitsWr i
--------------------
g permitsRd i
*/
addRule(new Rule.ReflectiveBackward() {
    override def toString = "PermitsWr-Implies-PermitsRd"
    
    def trigger(state: Network#State, fact: Fact.PermitsRd) = {
        val Fact.PermitsRd(g, i) = fact
        state.contains(Fact.PermitsWr(g, i))
    }
})

/*
g ensuresFinal i
--------------------
g permitsRd i
*/
addRule(new Rule.ReflectiveBackward() {
    override def toString = "EnsuresFinal-Implies-PermitsRd"
    
    def trigger(state: Network#State, fact: Fact.PermitsRd) = {
        val Fact.PermitsRd(g, i) = fact
        state.contains(Fact.EnsuresFinal(g, i))
    }
})

/*
i subOf j
g permitsRd j 
--------------------
g permitsRd i
*/
addRule(new Rule.ReflectiveBackward() {
    override def toString = "SubOf-PermitsRd"
    
    def trigger(state: Network#State, fact: Fact.PermitsRd) = {
        val Fact.PermitsRd(g, i) = fact
        state.allFacts(classOf[Fact.SubOf]).exists {
            case Fact.SubOf(i(), j) => {
                state.contains(Fact.PermitsRd(g, j))                
            }
            case _ => false
        }
    }
})

/*
i inlineSubOf j
g permitsWr j 
--------------------
g permitsWr i
*/
addRule(new Rule.ReflectiveBackward() {
    override def toString = "InlineSubOf-PermitsWr"
    
    def trigger(state: Network#State, fact: Fact.PermitsWr) = {
        val Fact.PermitsWr(g, i) = fact
        state.allFacts(classOf[Fact.InlineSubOf]).exists {
            case Fact.InlineSubOf(i(), j) => {
                state.contains(Fact.PermitsWr(g, j))
            }
            case _ => false
        }
    }
})

/*
--------------------
final ensuresFinal i
*/
addRule(new Rule.ReflectiveBackward() {
    override def toString = "Final-EnsuresFinal"
    
    def trigger(state: Network#State, fact: Fact.EnsuresFinal) = {
        fact.left.is(Path.Final)
    }
})
