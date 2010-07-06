package harmonic.compiler

import scala.util.parsing.input.NoPosition

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.immutable.Queue
import scala.collection.mutable

import com.smallcultfollowing.lathos.model.Page
import com.smallcultfollowing.lathos.model.Context
import com.smallcultfollowing.lathos.model.PageContent
import com.smallcultfollowing.lathos.model.Output
import com.smallcultfollowing.lathos.model.{Util => LathosUtil}

import Util._
import Error.CanFail

/*

The Env class defines our type system.
It collects facts and also embodies the rules
for computing new ones.  

The most complex rule manipulation is done by 
the `Env.ProofState` class, along with the
`Env.RuleTemplate` and `Env.Rule` instances.
This is a prolog-like engine which begins
with a query and a set of facts.  The `ProofState`
object then iterates through the various RuleTemplate
instances that are defined, supplying them with the
query at hand and asking them to contribute to the 
fact database.  Once the set of facts reaches a 
steady state, the query can be answered
(for monotonic queries, the answer can be found
sooner).

*/

object Env {
    def empty(global: Global) = Env(
        global      = global,
        thisTy      = Type.Top,
        optReturnTy = None,
        locals      = Map(Name.FinalLocal -> global.finalSym),
        facts       = Set()
    )
    
    // ___ Queries __________________________________________________________
    //
    // Subtypes of `Query` abstractly represent the queries that can be 
    // issued and evaluated by the environment.  They correspond to different
    // kinds of judgements in the inference rules.
    //
    // There are a number of subtypes of Query specifying what 
    // information is given and what is requested.  For example, a
    // Query.GivenPathRel indicates that we were given a 
    // path `p` and a relation `rel`, and are asked to find all paths `q`
    // such that `p rel q`.
    //
    // Most rules match using the extractors given short acronym names,
    // like Query.PR, which matches any kind of query where the domain path
    // and relation name have been given.  This could be a Query.GivenPathRel
    // or a Query.GivenPathRelPath.

    sealed abstract class Query[R] {
        // Invoked periodically while gathering facts.  If
        // it returns `Some(r)`, then we stop gathering facts
        // and just return `r`.  Always safe to return `None`.
        def preevaluate(facts: Set[Fact]): Option[R]
        
        // Once all facts have been gathered, evalutes the
        // and returns the result of the query.
        def evaluate(facts: Set[Fact]): R
    }
    
    object Query {
        abstract class FindFact(desiredFact: Fact) extends Query[Boolean] {
            override def preevaluate(facts: Set[Fact]) = {
                if(facts(desiredFact)) Some(true)
                else None
            }
            
            override def evaluate(facts: Set[Fact]) = {
                facts(desiredFact)
            }
        }
        
        // Query to find if `(left rel right)` can be proven.
        case class GivenBothPaths(left: Path.Ref, rel: PcRel, right: Path.Ref) 
        extends FindFact(Fact.PP(left, rel, right))
        
        // Query for all paths R where `left rel R` can be proven.
        // Only possible for transitive relations like 'hb' or 'eq'.
        case class GivenPathRel(left: Path.Ref, rel: PcTransRel) extends Query[Set[Path.Ref]] {
            override def preevaluate(facts: Set[Fact]) = None
            
            override def evaluate(facts: Set[Fact]) = {
                facts.flatMap {
                    case Fact.PP(left(), rel(), r) => Some(r)
                    case _ => None                    
                }
            }            
        }

        // Query for all paths L where `L rel right` can be proven
        // Only possible for transitive relations like 'hb' or 'eq'.
        case class GivenRelPath(rel: PcTransRel, right: Path.Ref) extends Query[Set[Path.Ref]] {
            override def preevaluate(facts: Set[Fact]) = None
            
            override def evaluate(facts: Set[Fact]) = {
                facts.flatMap {
                    case Fact.PP(l, rel(), right()) => Some(l)
                    case _ => None                    
                }
            }            
        }
        
        object PRP { // "Path Rel Path"
            def unapply(query: Query[_]) = query match {
                case GivenBothPaths(l, rel, r) => Some((l, rel, r))
                case _ => None
            }
        }
        
        object PR { // "Path Rel"
            def unapply(query: Query[_]) = query match {
                case GivenBothPaths(l, rel, _) => Some((l, rel))
                case GivenPathRel(l, rel) => Some((l, rel))
                case _ => None
            }
        }
        
        object RP { // "Rel Path"
            def unapply(query: Query[_]) = query match {
                case GivenBothPaths(_, rel, r) => Some((rel, r))
                case GivenRelPath(rel, r) => Some((rel, r))
                case _ => None
            }
        }
                
        case class GivenBothTypes(left: Type, rel: TcRel, right: Type) 
        extends FindFact(Fact.TT(left, rel, right))
        
        case class GivenLeftType(left: Type, rel: TcRel) extends Query[Set[Type]] {
            override def preevaluate(facts: Set[Fact]) = None
            
            override def evaluate(fact: Fact) = fact match {
                case Fact.TT(left(), rel(), r) => Some(r)
                case _ => None
            }            
        }
        
        case class GivenRightType(rel: TcRel, right: Type) extends Query[Set[Type]] {
            override def preevaluate(facts: Set[Fact]) = None
            
            override def evaluate(fact: Fact) = fact match {
                case Fact.TT(l, rel(), right()) => Some(l)
                case _ => None
            }            
        }
        
        object TRT { // "Type Rel Type"
            def unapply(query: Query[_]) = query match {
                case GivenBothTypes(l, rel, r) => Some((l, rel, r))
                case _ => None
            }
        }
        
        object TR { // "Type Rel"
            def unapply(query: Query[_]) = query match {
                case GivenBothTypes(l, rel, _) => Some((l, rel))
                case GivenLeftType(l, rel) => Some((l, rel))
                case _ => None
            }
        }

        object RT { // "Rel Type"
            def unapply(query: Query[_]) = query match {
                case GivenBothTypes(_, rel, r) => Some((rel, r))
                case GivenRightType(rel, r) => Some((rel, r))
                case _ => None
            }
        }
    }
    
    // ___ High-level query evaluation ______________________________________
    //
    // Queries are evaluated at a high-level by instantiating and applying
    // inference rules, represented as instances of Rule.  These inference
    // rules are applied repeatedly until the set of facts reaches a fixed
    // point. This is highly unoptimized at the moment.

    /** Represents an instantiation of an inference rule with (partially)
      * bound parameters.  To avoid infinite loops, two rules must be equal
      * if they would perform the same computation steps. */
    sealed abstract trait Rule {
        def deriveFacts(state: ProofState): Iterable[Fact]
    }
    
    /** Rule templates instantiate rules that are likely to produce facts
      * useful to answering `query`. */
    sealed abstract trait RuleTemplate {
        /** Returns a list of rules that might produce facts useful to answer
          * `query`.  These rules usually are specific to information in the
          * query. */
        def instantiate(state: ProofState)(query: Query[_]): Iterable[Rule]
        
        // Add ourselves to this list upon instantiation.  This is not really
        // a great idea, except that we only instantiate RuleTemplates as 
        // objects within this class:
        templates = this :: templates
    }
    
    private[this] var templates: List[RuleTemplate] = Nil
    
    /** Records the current state of the computation (what facts are known,
      * which rules are in the process of being evaluated, etc).  Right
      * now we don't make much use of this object, but any recursive queries
      * used during rule evaluation <b>must</b> use `state.answer()` rather
      * than the `answer()` method offered on the environment. */
    case class ProofState(
        global: Global,
        stack: List[Rule],
        locals: Map[Name.LocalVar, VarSymbol.Local],
        facts: Set[Fact]
    ) {
        private[this] def evaluateRule(rule: Rule) = {
            rule.deriveFacts(copy(stack = rule :: stack))
        }
        
        def answer[R](query: Query[R]): R = {
            // pre-evaluate allows the query to stop early if it has 
            // enough facts to be answered, even if more facts might be
            // computable
            query.preevaluate(facts) match {
                case Some(r) => r
                case None => {
                    // Evaluation of the rules could be easily parallelized if it would be helpful:
                    val newFacts = templates.foldLeft(facts) { (f, template) =>
                        val rules = template.instantiate(this)(query).filterNot(stack.contains)
                        rules.foldLeft(f) { (f, rule) => f ++ evaluateRule(rule) }
                    }

                    // Did we learn any new facts?  If so, loop as we might yet learn more.
                    if(newFacts.size != facts.size) {
                        copy(facts = newFacts).answer(query)
                    } else {
                        query.evaluate(facts)
                    } 
                }
            }
        }
        
        def rel(p: Path.Ref, rel: PcTransRel): Set[Path.Ref] = answer(Query.GivenPathRel(p, rel))
        def rel(rel: PcTransRel, q: Path.Ref): Set[Path.Ref] = answer(Query.GivenRelPath(rel, q))
        def rel(p: Path.Ref, rel: PcRel, q: Path.Ref): Boolean = answer(Query.GivenBothPaths(p, rel, q))

        def rel(l: Type, rel: TcRel, r: Type): Boolean = answer(Query.GivenBothTypes(l, rel, r))
        def rel(l: Type, rel: TcRel): Set[Type] = answer(Query.GivenLeftType(l, rel))
        def rel(rel: TcRel, r: Type): Set[Type] = answer(Query.GivenRightType(rel, r))
        
        def fact(f: Fact) = f match {
            case Fact.PP(p1, r, p2) => rel(p1, r, p2)
            case Fact.TT(t1, r, t2) => rel(t1, r, t2)
        }
        
        def equiv(p: Path.Ref): Set[Path.Ref] = rel(p, PcEq)
        def equiv(p: Path.Ref, q: Path.Ref): Boolean = rel(p, PcEq, q)

        def equiv(ty: Type): Set[Type] = rel(ty, TcEq)
        def equiv(ty1: Type, ty2: Type): Boolean = rel(ty1, TcEq, ty2)
        
        def parents(c: Path.Ref): Set[Path.Ref] = rel(c, PcSubOf)
        def inlineParents(c: Path.Ref): Set[Path.Ref] = rel(c, PcInlineSubOf)
        def children(p: Path.Ref): Set[Path.Ref] = rel(PcSubOf, p)
        def inlineChildren(p: Path.Ref): Set[Path.Ref] = rel(PcInlineSubOf, p)
        
        def permitsWr(g: Path.Ref, i: Path.Ref): Boolean = rel(g, PcPermitsWr, i)
        def permitsRd(g: Path.Ref, i: Path.Ref): Boolean = rel(g, PcPermitsRd, i)
        def ensuresFinal(g: Path.Ref, i: Path.Ref): Boolean = rel(g, PcEnsuresFinal, i)

        /** Convenience method for finding equatable path lists of `ps`
          * If `ps` is `(x, y, z)`, yields something like
          * `((x0, y0, z0), (x1, y0, z0), ...)` where `x0 eq x` etc. */
        def equivs(ps: List[Path.Ref]): List[List[Path.Ref]] = {
            ps match {
                case p :: tl => {
                    val eqTls = equivs(tl)
                    equiv(p).toList.flatMap { hd =>
                        eqTls.map(hd :: _)
                    }
                }

                case Nil => Nil
            }
        }
        
        // ___ Typed Paths ______________________________________________________   
        //
        // Strictly speaking, finding the type for a path is a relation, and we
        // could have introduced a Fact to represent it.  However, unlike other
        // facts, typing a path is a deterministic process and thus we use a
        // more straight-forward mechanism for evaluating it.

        def typedOwner(owner: Path.UntypedOwner): Path.TypedOwner = owner match {
            case Path.Static => Path.Static
            case owner: Path.Ref => typedPath(owner)
        }

        def typedPath(path: Path.Ref): Path.Typed = path match {
            case Path.Local(name: Name.LocalVar) => {
                val lvsym = locals.get(name).getOrElse(VarSymbol.errorLocal(name, None))
                Path.TypedLocal(lvsym)
            }

            case Path.Field(owner, name) => {
                val fsym = global.fieldSymbol(name).getOrElse {
                    // TODO --- report invalid method ids at the site of use.
                    Error.NoSuchField(name).report(
                        global, 
                        InterPosition.forClassNamed(methodId.className)
                    )
                    VarSymbol.errorField(name, None)
                }
                Path.TypedField(typedOwner(owner), fsym)
            }

            case Path.Call(owner, methodId, args) => {
                val csym = global.csym(methodId.className)
                val msym = csym.methodsNamed(methodId.methodName).find(_.id.is(methodId)).getOrElse {
                    // TODO --- report invalid method ids at the site of use.
                    Error.NoSuchMethodId(methodId).report(
                        global, 
                        InterPosition.forClassNamed(methodId.className)
                    )
                    MethodSymbol.error(methodId)
                }
                Path.TypedCall(typedOwner(owner), msym, args.map(typedPath))
            }

            case Path.Cast(ty, base) => {
                Path.TypedCast(ty, typedPath(base))
            }

            case Path.Constant(obj) => {
                Path.TypedConstant(obj)
            }

            case Path.Index(array, index) => {
                Path.TypedIndex(typedPath(array), typedPath(index))
            }

            case Path.Tuple(paths) => {
                Path.TypedTuple(paths.map(typedPath))
            }
        }

        def typeOfPath(path: Path.Ref) = typedPath(path).ty
        
        def upcast(p: Path.Ref, c: Name.Class): Set[Type.Class] = {
            rel(typeOfPath(p), TcSub).flatMap {
                case ty @ Type.Class(c(), _) => Some(ty)
                case _ => None
            }
        }
        
    }
    
    // ___ Rules ____________________________________________________________

    // ______ Type Arguments ________________________________________________
    
    /*
    p : c[..., f rel q, ...]
    --------------------
    p.(c.f) rel q
    */
    object TypeArgsTemplate extends RuleTemplate {
        case class TypeArgs(p: Path.Ref, c: Name.Class) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.upcast(p, c).flatMap { case Type.Class(_, args) =>
                    args.map {
                        case Type.PathArg(f, rel, q) => Fact.PP(p / f, rel, q)
                        case Type.TypeArg(f, rel, t) => Fact.TT(Type.Member(p, f), rel, t)
                    }
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PR(Path.Field(p, f), _) => Some(TypeArgs(p, f.className))
            case _ => None
        }
    }
        
    // ______ Equivalence Propagation _______________________________________
    //
    // The `Eq` relation has the nice property that any two paths that are
    // equivalent are also equivalent with respect to all other relations.
    // The following three templates implement this rule.  
    
    /*
    p == q
    q rel r
    --------------------
    p rel r
    
    Covers all cases where both p, r are known.
    */
    object PathEqPropTemplate extends RuleTemplate {
        case class PathEqProp(p: Path.Ref, rel: PcRel, r: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.equiv(p).flatMap { q =>
                    state.rel(q, rel, r) match {
                        case true => Some(Fact.PP(p, rel, r))
                        case false => None
                    }
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PRP(p, rel, r) => Some(PathEqProp(p, rel, r))
            case _ => None
        }
    }    
    
    /*
    p == q
    q trel r
    --------------------
    p trel r
    
    Covers cases where only p is known.
    */
    object PathEqLPropTemplate extends RuleTemplate {
        case class PathEqLProp(p: Path.Ref, rel: PcTransRel) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.equiv(p).flatMap { q =>
                    state.rel(q, rel).map { r =>
                        Fact.PP(p, rel, r)
                    }
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.GivenPathRel(p, rel) => Some(PathEqLProp(p, rel))
            case _ => None
        }
    }
    
    /*
    p == q
    q trel r
    --------------------
    p trel r
    
    Covers cases where only r is known.
    */
    object PathEqRPropTemplate extends RuleTemplate {
        case class PathEqRProp(rel: PcTransRel, r: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.equiv(r).flatMap { q =>
                    state.rel(rel, q).map { p =>
                        Fact.PP(p, rel, r)
                    }
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.GivenRelPath(rel, r) => Some(PathEqRProp(rel, r))
            case _ => None
        }
    }
    
    // ______ Path Equality _________________________________________________

    /*
    p == q
    --------------------
    q == p
    */
    object PathEqSymmetricTemplate extends RuleTemplate {
        case class PathEqSymmetric(right: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.equiv(right).map(Fact.PP(_, PcEq, right))
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.RP(PcEq, right) => Some(PathEqSymmetric(right))
            case _ => None
        }
    }
    
    /*
    --------------------
    (T)p == p
    etc
    
    Also handles the reflexive case (p == p).
    */
    object PathSimplifyTemplate extends RuleTemplate {
        def simplify(p: Path.Ref): Path.Ref = {
            p match {
                case Path.Cast(_, base) => base
                case Path.Tuple(List(q)) => simplify(q)
                case Path.Index(
                    Path.Tuple(paths), 
                    Path.Constant(index: java.lang.Integer)
                ) if index.intValue < paths.length => {
                    simplify(paths(index.intValue))
                }
                case _ => p
            }
        }
                
        case class PathSimplify(p: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                List(
                    Fact.PP(p, PcEq, p), // toss in that eq. is reflexive
                    Fact.PP(p, PcEq, simplify(p))
                )
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PR(left, PcEq) => Some(PathSimplify(left))
            case _ => None
        }
    }    
    
    /*
    p == q
    --------------------
    p.f == q.f
    etc
    */
    object PathEqInductiveTemplate extends RuleTemplate {
        case class PathEqInductive(path: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                import state.equiv
                import state.equivs
                
                val equivPaths = path match {
                    case Path.Field(base: Path.Ref, name) => {
                        equiv(base).map(Path.Field(_, name))
                    }
                    
                    case Path.Cast(t, base) => {
                        equiv(base).map(Path.Cast(t, _))
                    }

                    case Path.Index(array, index) => {
                        (equiv(array) cross equiv(index)).map { case (a, i) =>
                            Path.Index(a, i)
                        }
                    }

                    case Path.Tuple(paths) => {
                        equivs(paths).map(Path.Tuple)
                    }

                    case Path.Call(receiver: Path.Ref, methodName, args) => {
                        (equiv(receiver) cross equivs(args)).map { case (r, a) =>
                            Path.Call(r, methodName, a)
                        }
                    }
                    
                    case Path.Call(Path.Static, methodName, args) => {
                        equivs(args).map { a =>
                            Path.Call(Path.Static, methodName, a)
                        }
                    }

                    case Path.Field(Path.Static, _) 
                    |   Path.Local(_)
                    |   Path.Constant(_) => Set()
                }
                
                equivPaths.map(q => Fact.PP(path, PcEq, q))
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PR(p, PcEq) => Some(PathEqInductive(p))
            case _ => Nil
        }
    }

    // ______ Type Equality _________________________________________________

    /*
    t1 == t2
    --------------------
    t2 == t1
    */
    object TypeEqSymmetricTemplate extends RuleTemplate {
        case class TypeEqSymmetric(t1: Type) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.equiv(t1).map(t2 => Fact.TT(t2, TcEq, t1))
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.RT(TcEq, t1) => Some(TypeEqSymmetric(t1))
            case _ => None
        }
    }
    
    /*
    --------------------
    (t) == t
    etc
    
    Also handles the reflexive case (t == t).
    */
    object TypeSimplifyTemplate extends RuleTemplate {
        def simplify(ty: Type): Type = {
            ty match {
                case Type.Tuple(List(ty)) => simplify(ty)   // (t) == t
                case Type.Tuple(Nil) => Type.Void           // () == Void, not sure if this makes sense.
                case ty => ty
            }
        }
                
        case class TypeSimplify(ty: Type) extends Rule {
            override def deriveFacts(state: ProofState) = {
                List(
                    Fact.TT(ty, TcEq, ty),
                    Fact.TT(ty, TcEq, simplify(ty))
                )
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.TR(left, TcEq) => Some(TypeSimplify(left))
            case _ => None
        }
    }
    
    /*
    p == q
    --------------------
    p.X == q.X
    where p/q are paths, X is a type variable
    */
    object MemberTypeInductiveTemplate extends RuleTemplate {
        case class MemberTypeInductive(p: Path.Ref, x: Name.Member) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.equiv(p).toList.map { q =>
                    Fact.TT(Type.Member(p, x), TcEq, Type.Member(q, x))
                }
            }
        }        
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.TR(Type.Member(p, x), TcEq) => Some(MemberTypeInductive(p, x))
            case _ => None
        }
    }

    // ______ Sub- and super-typing _________________________________________

    /*
    t2 <: t1
    --------------------
    t1 :> t2
    */
    object SupFromSupTemplate extends RuleTemplate {
        case class SupFromSub(t2: Type) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.rel(t2, TcSub).map(t1 => Fact.TT(t1, TcSup, t2))
            }
        }        
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.RT(TcSup, t2) => Some(SupFromSub(t2))
            case _ => None
        }
    }
    
    /*
    class c1 extends c2
    --------------------
    c1[args] <: c2[args]
    */
    object ExtendsTemplate extends RuleTemplate {
        case class Extends(ty1: Type.Class) extends Rule {
            override def deriveFacts(state: ProofState) = {
                val csym = state.global.csym(ty1.name)
                csym.superClassNames.map { c2 =>
                    // TODO: Add in any applicable constraints defined in c1!!
                    // TODO: Filter out inapplicable arguments from ty (won't hurt though)
                    Fact.TT(ty1, TcSub, Type.Class(c2, ty1.args))
                }
            }
        }        
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.TR(ty1: Type.Class, TcSub) => Some(Extends(ty1))
            case _ => None
        }
    }
    
    /*
    t1 <: t2
    t2 <: t3
    --------------------
    t1 <: t3
    */
    object SubtypingTransTemplate extends RuleTemplate {
        case class SubtypingTrans(ty1: Type.Class) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.rel(ty1, TcSub).flatMap { ty2 =>
                    state.rel(ty2, TcSub).map { ty3 =>
                        Fact.TT(ty1, TcSub, ty3)
                    }
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.TR(ty1: Type.Class, TcSub) => Some(Extends(ty1))
            case _ => None
        }
    }
    
    /*
    t1 == t2
    --------------------
    t1 <: t2
    */
    object EqualityImpliesSubtypingTemplate extends RuleTemplate {
        case class EqualityImpliesSubtyping(ty1: Type.Class) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.rel(ty1, TcEq).map { ty2 =>
                    Fact.TT(ty1, TcSub, ty2)
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.TR(ty1: Type.Class, TcSub) => Some(Extends(ty1))
            case _ => None
        }
    }    
    
    // ______ subOf, inlineSubOf, hb ________________________________________

    /*
    pc inlineSubOf pp
    --------------------
    pc subOf pp
    */
    object InlineImpliesSubTemplate extends RuleTemplate {
        case class InlineImpliesSub(pc: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.inlineParents(pc).map { pp =>
                    Fact.PP(pc, PcSubOf, pp)
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PR(pc, PcSubOf) => Some(InlineImpliesSub(pc))
            case _ => None
        }
    }
    
    /*
    pc subOf pp
    ------------
    pp.start hb pc.start
    pc.end hb pp.end
    */
    object ParentHbTemplate extends RuleTemplate {
        import MethodId.GetStart
        import MethodId.GetEnd
        
        case class ParentToChild(pp: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.children(pp).map(pc => Fact.PP(pp.call(GetStart), PcHb, pc.call(GetStart)))
            }
        }
        
        case class ChildToParent(pc: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.parents(pc).map(pp => Fact.PP(pc.call(GetEnd), PcHb, pp.call(GetEnd)))
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PR(Path.Call(pp: Path.Ref, GetStart, List()), PcHb) => Some(ParentToChild(pp))
            case Query.PR(Path.Call(pc: Path.Ref, GetEnd, List()), PcHb) => Some(ChildToParent(pc))
            case _ => None
        }
    }    
    
    // ______ permitsWr, permitsRd, ensuresFinal ____________________________

    /*
    g permitsWr i
    --------------------
    g permitsRd i
    */
    object PermitsWrImpliesRdTemplate extends RuleTemplate {
        case class PermitsWrImpliesRd(g: Path.Ref, i: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.permitsWr(g, i) match {
                    case true => Some(Fact.PP(g, PcPermitsRd, i))
                    case false => None
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PRP(g, PcPermitsRd, i) => Some(PermitsWrImpliesRd(g, i))
            case _ => None
        }
    }
    
    /*
    g ensuresFinal i
    --------------------
    g permitsRd i
    */
    object EnsuresFinalImpliesRdTemplate extends RuleTemplate {
        case class EnsuresFinalImpliesRd(g: Path.Ref, i: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.ensuresFinal(g, i) match {
                    case true => Some(Fact.PP(g, PcPermitsRd, i))
                    case false => None
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PRP(g, PcPermitsRd, i) => Some(EnsuresFinalImpliesRd(g, i))
            case _ => None
        }
    }

    /*
    i subOf j
    g permitsRd j 
    --------------------
    g permitsRd i
    */
    object PermitsRdToSubTemplate extends RuleTemplate {
        case class PermitsRdToSub(g: Path.Ref, i: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.parents(i).flatMap { j =>
                    state.permitsRd(g, j) match {
                        case true => Some(Fact.PP(g, PcPermitsRd, i))
                        case false => None
                    }
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PRP(g, PcPermitsRd, i) => Some(PermitsRdToSub(g, i))
            case _ => None
        }
    }
    
    /*
    i inlineSubOf j
    g permitsWr j 
    --------------------
    g permitsWr i
    */
    object PermitsWrToInlineSubTemplate extends RuleTemplate {
        case class PermitsWrToInlineSub(g: Path.Ref, i: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                state.inlineParents(i).flatMap { j =>
                    state.permitsWr(g, j) match {
                        case true => Some(Fact.PP(g, PcPermitsWr, i))
                        case false => None
                    }
                }
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PRP(g, PcPermitsWr, i) => Some(PermitsWrToInlineSub(g, i))
            case _ => None
        }
    }
    
    /*
    --------------------
    final ensuresFinal i
    */
    object FinalEnsuresFinalTemplate extends RuleTemplate {
        case class FinalEnsuresFinal(i: Path.Ref) extends Rule {
            override def deriveFacts(state: ProofState) = {
                Some(Fact.PP(Path.Final, PcEnsuresFinal, i))
            }
        }
        
        def instantiate(state: ProofState)(query: Query[_]) = query match {
            case Query.PRP(_, PcEnsuresFinal, i) => Some(FinalEnsuresFinal(i))
            case _ => None
        }
    }
}

/** The environment is used during a type check but also in other phases
  * It stores information known by the compiler. */
case class Env(
    global: Global,
    
    /** Type of the this pointer */
    thisTy: Type.Class,
    
    /** Return type at current point, or 
      * None if returns not currently allowed */
    optReturnTy: Option[Type],
    
    /** In-scope local variables. */
    locals: Map[Name.LocalVar, VarSymbol.Local],
    
    /** Known facts */
    facts: Set[Fact]
) extends Page {
    
    private[this] def curLog = global.closestLog
    
    override def toString = getId
    
    // ___ Page interface ___________________________________________________
    
    override def getId = "Env[%s]".format(System.identityHashCode(this))
    
    override def getParent = null
    
    override def addContent(content: PageContent) = throw new UnsupportedOperationException()
    
    override def renderInLine(out: Output): Unit = {
        LathosUtil.renderInLine(this, out)
    }
    
    override def renderInPage(out: Output): Unit = {
        out.startPage(this)
        
        out.startTable
        
        out.row("thisTy", thisTy)
        out.row("optReturnTy", optReturnTy)
        
        out.endTable
        
        out.subpage("Locals") {
            out.startTable
            for((name, sym) <- locals){
                out.row(name, sym, sym.ty)
            }
            out.endTable
        }

        out.subpage("Facts") {
            out.list(facts)
        }
        
        out.endPage(this)
    }

    // ___ Extending the Environment ________________________________________
    
    def plusLocalVar(sym: VarSymbol.Local) = copy(locals = locals + (sym.name -> sym))
    
    def plusLocalVars(syms: Iterable[VarSymbol.Local]) = syms.foldLeft(this)(_ plusLocalVar _)

    def plusThis(thisTy: Type.Class, sym: VarSymbol.Local) = plusLocalVar(sym).copy(thisTy = thisTy)
    
    def plusFact(fact: Fact) = copy(facts = facts + fact)
    
    def plusFacts(facts: Iterable[Fact]) = facts.foldLeft(this)(_ plusFact _)
    
    def withOptReturnTy(optReturnTy: Option[Type]) = copy(optReturnTy = optReturnTy)
    
    // ___ Simple queries ___________________________________________________
    
    val state = Env.ProofState(global, Nil, locals, facts)
    
    def typesAreEquatable(ty1: Type, ty2: Type) = state.equiv(ty1, ty2)
    
    def pathsAreEquatable(p1: Path.Ref, p2: Path.Ref) = state.equiv(p1, p2)
    
    def typeOfPath(path: Path.Ref) = state.typeOfPath(path)
    
    def typedPath(path: Path.Ref) = state.typedPath(path)
    
    def factHolds(fact: Fact): Boolean = state.fact(fact)

    // ___ Finding member names _____________________________________________
    
    def lookupEntry(csym: ClassSymbol, uName: Name.UnloweredMember): CanFail[SymTab.MemberEntry] = {
        val mro = csym.mro

        // Find all entries that could match `uName`:
        val allEntries = mro.flatMap { mrosym => 
            mrosym.varMembers.flatMap(_.asMemberEntryMatching(uName)) 
        }

        // Try to find if there is one that shadows all others:
        allEntries match {
            case List() => Left(Error.NoSuchMember(csym.toType, uName))
            case List(entry) => Right(entry)
            case entry :: otherEntries => {
                val csym = global.csym(entry.name.className)
                val remEntries = otherEntries.filterNot { entry =>
                    csym.isSubclass(global.csym(entry.name.className))
                }
                if(remEntries.isEmpty) {
                    Right(entry)
                } else {
                    Left(Error.AmbiguousMember(entry :: remEntries))                    
                }
            }
        }            
    }
    
    // ___ Looking up fields and methods ____________________________________
    //
    // Note: this can trigger lowering to occur!  
    
    def localIsDefined(name: Name.LocalVar) = 
        locals.isDefinedAt(name)
    
    def lookupThis = 
        locals(Name.ThisLocal)
    
    def thisCsym = 
        global.csym(thisTy.name)
        
    private[this] def lookupMember[R](
        ownerTy: Type, 
        uName: Name.UnloweredMember
    )(
        func: (SymTab.MemberEntry => CanFail[R])
    ): CanFail[R] = {
        minimalUpperBounds(ownerTy).firstRight[Error, R](Error.NoSuchMember(ownerTy, uName)) {
            case (_, Type.Class(className, _)) => {
                val csym = global.csym(className)
                lookupEntry(csym, uName) match {
                    case Left(err) => Left(err)
                    case Right(entry) => func(entry)
                }
            }
            case (err, _) => Left(err)
        }
    }
    
    def lookupTypeVar(
        ownerTy: Type, 
        uName: Name.UnloweredMember
    ): CanFail[Name.Member] = {
        lookupMember(ownerTy, uName) {
            case SymTab.Type(memberVar) => Right(memberVar)
            case entry => Left(Error.NotTypeVar(entry))
        }
    }

    private[this] def lookupField(
        ownerTy: Type, 
        uName: Name.UnloweredMember
    ): CanFail[VarSymbol.Field] = {
        def findSym(memberVar: Name.Member) = {
            val memberCsym = global.csym(memberVar.className)
            memberCsym.fieldNamed(memberVar).orErr(Error.NoSuchMember(ownerTy, uName))                
        }
        
        lookupMember(ownerTy, uName) {
            case SymTab.InstanceField(memberVar) => findSym(memberVar)
            case SymTab.StaticField(memberVar) => findSym(memberVar)
            case entry => Left(Error.NotField(entry.name))
        }
    }
    
    def lookupBean(
        ownerTy: Type, 
        uName: Name.UnloweredMember
    ): CanFail[Either[VarSymbol.Field, MethodSymbol]] = {
        (lookupField(ownerTy, uName), uName) match {
            // If no field `foo` is found, try to find a method `getFoo`.
            case (Left(err @ Error.NoSuchMember(_, _)), Name.ClasslessMember(text)) => {
                val prop = "get%c%s".format(text.charAt(0).toUpper, text.substring(1))
                val methodName = Name.Method(List(prop))
                lookupInstanceMethods(ownerTy, methodName) match {
                    case Nil => Left(err)
                    case msym :: _ => Right(Right(msym))
                }
            }
            
            case (Left(err), _) => Left(err)
            
            case (Right(fld), _) => Right(Left(fld))
        }
    }
        
    private[this] def lookupFieldOrError(
        ownerTy: Type,  
        name: Name.UnloweredMember,
        optExpTy: Option[Type]
    ): VarSymbol.Field = {
        lookupField(ownerTy, name) match {
            case Left(_) => VarSymbol.errorField(name.inDefaultClass(Name.ObjectClass), optExpTy)
            case Right(sym) => sym
        }
    }    
    
    private[this] def lookupInstanceMethodsDefinedOnClass(
        className: Name.Class,
        methodName: Name.Method
    ): List[MethodSymbol] = {
        global.lookupIntrinsic(className, methodName).getOrElse {
            val csym = global.csym(className)
            csym.methodsNamed(methodName).filterNot(_.modifiers.isStatic)
        }
    }
    
    def lookupInstanceMethods(
        rcvrTy: Type, 
        methodName: Name.Method
    ): List[MethodSymbol] = {
        minimalUpperBounds(rcvrTy).firstSome({ 
            case classTy: Type.Class => 
                val mro = global.csym(classTy.name).mro
                mro.firstSome { csym =>
                    lookupInstanceMethodsDefinedOnClass(csym.name, methodName) match {
                        case List() => None
                        case msyms => Some(msyms)
                    }                    
                }
            case _ => None
        }).getOrElse(List())
    }
    
    // ___ Bounding Type Variables __________________________________________
    
    def upperBounds(ty: Type) = state.rel(ty, TcSub)
    def lowerBounds(ty: Type) = state.rel(TcSub, ty)
    
    /** Returns a minimal set of upper-bounds for `ty`. "Minimal"
      * means that we remove redundant class types; i.e., if 
      * references to classes C and D are both in the list, and
      * C extends D, then D will be removed. */
    def minimalUpperBounds(ty: Type) = {
        val bnds = upperBounds(ty)
        bnds.foldLeft(bnds) { 
            case (b, classTy: Type.Class) => {
                val mro = global.csym(classTy.name).mro
                val purgeNames = Set(mro.tail.map(_.name): _*)
                b.filter {
                    case Type.Class(name, _) => !purgeNames(name)
                    case _ => true
                }
            }
            
            case (b, _) => b
        }
    }
    
    // ___ Mutual Upper-Bound _______________________________________________
    //
    // TODO Smarten up Mutual Upper Bound but be wary of infinite recursion.
    //
    // This could be smarter in a number of ways:
    // - List[E <: String], List[E <: Object] currently has List as its UB
    // - Type variables bounded by tuples end up as Object
    //
    // Probably more.  The reason for this is I don't want
    // to think too hard about infinite recursion.  Not yet anyhow.
    
    /** True if the type argument `arg` affects a member defined on `csym` */
    private[this] def appliesTo(csym: ClassSymbol)(arg: Type.Arg) = {
        val argClassName = arg.name.className
        val argCsym = global.csym(argClassName)
        csym.isSubclass(argCsym)
    }
    
    /** Returns type arguments only if both sides agree. */
    private[this] def intersectArgs(leftArgs: List[Type.Arg], rightArgs: List[Type.Arg]) = {
        val leftMap = leftArgs.map(arg => (arg.name, arg)).toMap
        rightArgs.filter(rightArg => {
            leftMap.get(rightArg.name) match {
                // No left arg by that name.  Drop it.
                case None => false 
                
                // Same arg on left.  Keep it.
                case Some(leftArg) if leftArg == rightArg => true 
                
                // Different arg on left.  For now, drop it.
                case Some(_) => false
            }
        })
    }
    
    private[this] def mutualUpperBoundVar(varTy: Type.Member, otherTy: Type) = {
        val ub = minimalUpperBounds(varTy)
        if(ub.contains(otherTy)) otherTy
        else ub.firstSome({
            case clsTy: Type.Class => Some(mutualUpperBound(clsTy, otherTy))
            case _ => None
        }).getOrElse(Type.Top)
    }
    
    private[this] def mutualUpperBoundUnmatchedTuple(tys: List[Type], otherTy: Type) = {
        val boundTy = mutualUpperBoundOfList(tys)
        mutualUpperBound((Type.arrayExtends(boundTy), otherTy))
    }
    
    def mutualUpperBoundOfList(tys: List[Type]): Type = {
        tys match {
            case List() => Type.Top
            case List(ty) => ty
            case tys => tys.reduceLeft { (a, b) => mutualUpperBound((a, b)) }
        }
    }
    
    /** Given a pair of types, returns a new type that is a supertype
      * of both.  Tries to pick a precise type when possible. */
    def mutualUpperBound(pair: (Type, Type)): Type = {
        pair match {
            case (leftTy, rightTy) if leftTy == rightTy =>
                leftTy
                
            // ___ null _____________________________________________________________
            
            case (Type.Null, _) => Type.Top
            
            case (_, Type.Null) => Type.Top
            
            // ___ tuples ___________________________________________________________
            
            case (Type.Tuple(List(leftTy)), rightTy) =>
                mutualUpperBound((leftTy, rightTy))
            
            case (leftTy, Type.Tuple(List(rightTy))) =>
                mutualUpperBound((leftTy, rightTy))
                
            case (Type.Tuple(leftTys), Type.Tuple(rightTys)) if sameLength(leftTys, rightTys) =>
                Type.Tuple(leftTys.zip(rightTys).map(mutualUpperBound))
                
            case (Type.Tuple(tys), otherTy) => 
                mutualUpperBoundUnmatchedTuple(tys, otherTy)
                
            case (otherTy, Type.Tuple(tys)) =>
                mutualUpperBoundUnmatchedTuple(tys, otherTy)
                
            // ___ type variables ___________________________________________________

            case (varTy: Type.Member, otherTy) => mutualUpperBoundVar(varTy, otherTy)

            case (otherTy, varTy: Type.Member) => mutualUpperBoundVar(varTy, otherTy)

            // ___ class types ______________________________________________________
            
            case (Type.Class(leftName, leftArgs), Type.Class(rightName, rightArgs)) => {
                // Find the most specific class symbol that is a supertype of both:
                // - We use MRO to decide what is more specific.  Note that this
                //   search can't fail: if will find Object, if nothing else.
                val leftCsym = global.csym(leftName)
                val rightCsym = global.csym(rightName)
                val leftMro = leftCsym.mro
                val bestCsym = leftMro.find(rightCsym.isSubclass(_)).get 
                
                // Restrict the arguments to those defined on that class symbol:
                // TODO Have to first fully elaborate left, right args
                val leftArgsUp = leftArgs.filter(appliesTo(bestCsym))
                val rightArgsUp = rightArgs.filter(appliesTo(bestCsym))
                val bestArgs = intersectArgs(leftArgsUp, rightArgsUp)
                
                Type.Class(bestCsym.name, bestArgs)
            }
        }
    }
    
    // ___ Argument Suitability _____________________________________________
    //
    // Argument suitability is used to resolve overloaded arguments.  We do not
    // consider the full subtyping relation but rather only the erased type
    // and (to a limited extent) type variables.
    
    private[this] def isSuitableArgumentBounded(ty_val: Type, ty_pat: Type): Boolean = {
        (ty_val, ty_pat) match {
            case (Type.Class(name_val, _), Type.Class(name_pat, _)) => {
                val sym_val = global.csym(name_val)
                val sym_pat = global.csym(name_pat)
                sym_val.isSubclass(sym_pat)
            }
            
            case (Type.Member(path_val, tvar_val), Type.Member(path_pat, tvar_pat)) if tvar_val == tvar_pat =>
                state.equiv(path_val, path_pat)
                
            case (Type.Tuple(tys_val), Type.Tuple(tys_pat)) if sameLength(tys_val, tys_pat) =>
                tys_val.zip(tys_pat).forall { case (v, p) => isSuitableArgument(v, p) }
                
            case (Type.Tuple(List(ty)), _) =>
                isSuitableArgument(ty, ty_pat)
                
            case (_, Type.Tuple(List(ty))) =>
                isSuitableArgument(ty_val, ty)
                
            case (Type.Null, _) => 
                true
                
            case _ =>
                false
        }
    }
    
    def isSuitableArgument(ty_val: Type, ty_pat: Type): Boolean = {
        (upperBounds(ty_val) cross lowerBounds(ty_pat)).exists {
            case (u, l) => isSuitableArgumentBounded(u, l)
        }
    }
    
    // ___ Type Equality ____________________________________________________
    //
    // Two types can be equal either by being lexicographically equal or
    // though various equality relations.
    
    private[this] def typeArgsAreEquatable(targ1: Type.Arg, targ2: Type.Arg): Boolean = {
        (targ1 == targ2) || {
            (targ1, targ2) match {
                case (Type.TypeArg(name1, rel1, ty1), Type.TypeArg(name2, rel2, ty2)) =>
                    name1 == name2 && rel1 == rel2 && typesAreEquatable(ty1, ty2)
                    
                case (Type.PathArg(name1, rel1, path1), Type.PathArg(name2, rel2, path2)) =>
                    name1 == name2 && rel1 == rel2 && pathsAreEquatable(path1, path2)
                
                case _ => false
            }
        }
    }

    private[this] def typesAreEquatable1(pair: (Type, Type)): Boolean = {
        val (ty1, ty2) = pair
        (ty1 == ty2) || {
            (ty1, ty2) match {
                case (Type.Class(name1, targs1), Type.Class(name2, targs2)) if sameLength(targs1, targs2) => {
                    name1 == name2 && 
                    targs1.forall(a => 
                        targs2.exists(b => 
                            typeArgsAreEquatable(a, b)))
                }
                
                case (Type.Tuple(tys1), Type.Tuple(tys2)) if sameLength(tys1, tys2) => {
                    tys1.zip(tys2).forall { case (ty1, ty2) => 
                        typesAreEquatable(ty1, ty2) 
                    }                    
                }
                
                case _ => false
            }
        }
    }
    
    // ___ Method Override Checking _________________________________________
    //
    // One method overrides another if the types of its arguments are
    // the same, after performing whatever substitutions
    // are necessary.
    
    /** Adds mappings to fresh names for each variable defined in `pat` */
    private[this] def addFresh(
        subst: Subst,
        pat: Pattern.Ref
    ): Subst = {
        pat match {
            case Pattern.Tuple(pats) => pats.foldLeft(subst)(addFresh)
            case Pattern.Var(name, _) => {
                val freshName = Name.LocalVar("(env-%s)".format(global.freshInteger))
                subst + (name.toPath -> freshName.toPath)
            }
        }
    }
    
    /** Given a pair of patterns `(sub, sup)` returns a substitution
      * mapping the variables from the `sup` pattern to corresponding
      * variables in the `sub` pattern.  If there is no corresponding
      * variable in the `sub` pattern, maps the variable in `sup` to a
      * fresh name.
      *
      * Examples:
      * - (a, b) => (a -> b)
      * - ((a, b), (c, d)) => (a -> c, b -> d)
      * - ((a, b), c) => (c -> fresh)
      */
    private[this] def addOverrideSubst(
        subst: Subst,
        pair: (Pattern.Ref, Pattern.Ref)
    ): Subst = {
        pair match {
            case (Pattern.Tuple(List(pat_sub)), pat_sup) =>
                addOverrideSubst(subst, (pat_sub, pat_sup))
            
            case (pat_sub, Pattern.Tuple(List(pat_sup))) =>
                addOverrideSubst(subst, (pat_sub, pat_sup))
            
            case (Pattern.Var(name_sub, _), Pattern.Var(name_sup, _)) =>
                subst + (name_sub.toPath -> name_sup.toPath)
                
            case (Pattern.Tuple(pats_sub), Pattern.Tuple(pats_sup)) if sameLength(pats_sub, pats_sup) =>
                pats_sub.zip(pats_sup).foldLeft(subst)(addOverrideSubst)
                
            case (_, pat_sup) =>
                addFresh(subst, pat_sup)
        }
    }
    
    /** Returns true if a method with signature `msig_sub` defined
      * in the current class overrides a method with signature 
      * `msig_sup` defined in some supertype of the current class.
      *
      * The "current class" means the one whose relations are to
      * be found in the environment. */
    def overrides(
        msig_sub: MethodSignature[Pattern.Ref], 
        msig_sup: MethodSignature[Pattern.Ref]
    ) = {
        val pps_sub = msig_sub.parameterPatterns
        val pps_sup = msig_sup.parameterPatterns
        val subst = pps_sub.zip(pps_sup).foldLeft(Subst.empty)(addOverrideSubst)
        pps_sub.zip(pps_sup).forall { case (pp_sub, pp_sup) => 
            state.equiv(pp_sub.ty, subst.ty(pp_sup.ty))
        }
    }
    
//    def isSubclass(ty_sub: Type, ty_sup: Type): Boolean = {
//        (ty_sub, ty_sup) match {
//            case (Type.Member(path_sub, var_sub), Type.Member(path_sup, var_sup)) => false // FIXME
//            
//            case (Type.Class(name_sub, args_sub), Type.Class(name_sup, arg_sup)) => false // FIXME
//                
//            case (Type.Tuple(tys_sub), Type.Tuple(tys_sup)) => 
//                tys_sub.zip(tys_sup).forall { case (s, t) => isSubclass(s, t) }
//            case (Type.Null, _) => 
//                true
//            case _ => 
//                false
//        }
//    }
//    
//    def matchesByClass(pattern: Pattern.Ref, ty: Type): Boolean = {
//        (pattern, ty) match {
//            // Unpack singleton tuples:
//            case (_, Type.Tuple(List(subty))) => matches(pattern, subty)
//            case (Symbol.Tuple(List(subpattern)), _) => matches(subpattern, ty)
//            
//            // Unpack matching tuples:
//            case (Symbol.Tuple(subpatterns), Type.Tuple(subtys)) if sameLength(subpatterns, subtys) =>
//                subpatterns.zip(subtys).forall { case (p, t) => matches(p, t) }
//
//            // Check for singleton tuples:
//            case (Pattern.Var(_, patty), ty) => isSubclass(patty, ty)
//            case _ => false
//        }
//    }

    // ___ Path has type ____________________________________________________
    //
    // This is the Harmonic equivalent to a subtype check.
    
    def isSatisfiedForPath(path: Path.Typed)(arg: Type.Arg): Boolean = {
        curLog.indent(this, ".isSatisfiedForPath(", path, ")(", arg, ")") {
            arg match {
                case Type.PathArg(name, rel, path2) => {
                    val extPath = Path.Field(path.toPath, name)
                    state.rel(extPath, rel, path2)
                }
                case Type.TypeArg(name, rel, ty) => {
                    val extTy = Type.Member(path.toPath, name)
                    state.rel(extTy, rel, ty)
                }
            }            
        }
    }
    
    def pathHasType(path: Path.Typed, ty: Type): Boolean = {
        curLog.indent(this, ".pathHasType(", path, ", ", ty, ")") {
            val ubSubTys = upperBounds(path.ty)
            val lbSuperTys = lowerBounds(ty)
            (ubSubTys cross lbSuperTys).exists {
                case (Type.Null, _) => {
                    true
                }
            
                case (t1 @ Type.Class(subName, _), t2 @ Type.Class(supName, supArgs)) => {
                    subName.is(supName) && supArgs.forall(isSatisfiedForPath(path))
                }
            
                case (t1, t2) => {
                    t1.is(t2)
                }
            }
        }
    }
    
    def isSubtype(subTy: Type, supTy: Type): Boolean = {
        val tempSym = new VarSymbol.Local(
            NoPosition, 
            Modifier.Set.empty, 
            global.freshLocalName,
            subTy
        )
        plusLocalVar(tempSym).pathHasType(tempSym.toTypedPath, supTy)
    }
  
    // ___ Path is final by _________________________________________________
    //
    // Determines whether a given path has reached its final value by
    // the given interval.  The path `inter` is assumed to be final.
    
    def factIsFinalBy(fact: Fact, inter: Path.Typed) = {
        fact match {
            case Fact.PP(l, _, r) => pathIsFinalBy(typedPath(l), inter) && pathIsFinalBy(typedPath(r), inter)
            case Fact.TT(l, _, r) => typeIsFinalBy(l, inter) && typeIsFinalBy(r, inter)
        }
    }
    
    def typeIsFinalBy(ty: Type, inter: Path.Typed): Boolean = {
        ty match {
            case Type.Member(path, _) => pathIsFinalBy(typedPath(path), inter)
            case Type.Class(_, args) => args.forall(typeArgIsFinalBy(_, inter))
            case Type.Tuple(tys) => tys.forall(typeIsFinalBy(_, inter))
            case Type.Null => true
        }
    }
    
    def typeArgIsFinalBy(targ: Type.Arg, inter: Path.Typed): Boolean = {
        targ match {
            case Type.PathArg(_, _, path) => pathIsFinalBy(typedPath(path), inter)
            case Type.TypeArg(_, _, ty) => typeIsFinalBy(ty, inter)
        }
    }
    
    def ownerIsFinalBy(owner: Path.TypedOwner, inter: Path.Typed) = {
        owner match {
            case Path.Static => true
            case owner: Path.Typed => pathIsFinalBy(owner, inter)
        }        
    }
    
    def pathIsFinalBy(path: Path.Typed, inter: Path.Typed): Boolean = {
        def wr(path: Path.Ref) = Path.Field(path, Name.Wr)
        
        path match {
            case Path.TypedTuple(paths) => {
                paths.forall(pathIsFinalBy(_, inter))
            }
            
            case Path.TypedLocal(sym) => {
                if(sym.modifiers.isNotMutable) true
                else {
                    val guardPath = locals(Name.MethodLocal) // TODO: Configurable guard paths for locals
                    state.ensuresFinal(guardPath.toPath, inter.toPath)
                }
            }
                
            case Path.TypedCast(_, castedPath) => {
                pathIsFinalBy(castedPath, inter)
            }
            
            case Path.TypedConstant(_) => {
                true                
            }
                
            case Path.TypedField(base, fsym) => {
                ownerIsFinalBy(base, inter) && {                    
                    val guardPath = locals(Name.FinalLocal) // TODO: Guard path for fields
                    state.ensuresFinal(guardPath.toPath, inter.toPath)
                }
            }
            
            case Path.TypedCall(receiver, msym, args) => {
                ownerIsFinalBy(receiver, inter) &&
                args.forall(pathIsFinalBy(_, inter)) && {
                    // TODO: Allow finalBy annotations in method signature
                    false 
                }
            }
            
            case Path.TypedIndex(array, index) => {
                pathIsFinalBy(array, inter) &&
                pathIsFinalBy(index, inter) && {
                    val guardPath = wr(array.toPath)
                    state.ensuresFinal(guardPath, inter.toPath)
                }
            }
            
        }
    }
    
  
}