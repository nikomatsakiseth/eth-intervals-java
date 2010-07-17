package harmonic.compiler

import scala.util.parsing.input.NoPosition

import com.smallcultfollowing.lathos

import harmonic.compiler.inference.Network
import harmonic.compiler.inference.Fact
import harmonic.compiler.inference.Rule
import harmonic.compiler.inference.Recurse

import ch.ethz.intervals

import Util._

/** Defines the inference rules for the Harmonic type system. 
  * The facts which we operate over are defined in the K module. */
class HarmonicRulesNetwork(server: lathos.LathosServer)
extends Network[Env.Xtra](server)
{
    protected[this] override def precontains(fact: Fact): Iterable[Fact.Forward] = {
        fact match {
            case K.Paths(l, r) => List(K.PathExists(l), K.PathExists(r))
            case K.Types(l, r) => List(K.TypeExists(l), K.TypeExists(r))
            case K.HasType(l, r) => List(K.PathExists(l), K.TypeExists(r))
            case _ => Nil
        }
    }
    
    protected[this] override def prequery(kind: Fact.Kind, args: Array[Option[Any]]): Iterable[Fact.Forward] = {
        args.view.flatMap {
            case Some(path: Path.Ref) => Some(K.PathExists(path))
            case Some(ty: Type) => Some(K.TypeExists(ty))
            case _ => None
        }
    }
    
    // ______ Path and Type facts ___________________________________________
    //
    // These are kind of a workaround that allows us to avoid using backwards
    // propagation and its inherent complications with transitive relations.
    //
    // Basically we add a Path or Type fact for every referenced type.  
    // This is then used to trigger various forward rules that handle 
    // "0-predicate" rules.

    class PathFactRule(
        val kind: Class[_ <: (K.Paths with Fact.Forward)]
    ) extends Rule.Forward[Env.Xtra] {
        override def toString = "PathFactRule(%s)".format(kind)
        val inputKinds = List(kind)
    
        override def derive(xtra: Env.Xtra, facts: List[Fact.Forward]): Iterable[Fact.Forward] = {
            val List(K.Paths(l, r)) = facts
            List(K.PathExists(l), K.PathExists(r))
        }
    }
    addRule(new PathFactRule(classOf[K.PathEq]))
    addRule(new PathFactRule(classOf[K.Hb]))
    addRule(new PathFactRule(classOf[K.SubOf]))
    addRule(new PathFactRule(classOf[K.InlineSubOf]))
    addRule(new PathFactRule(classOf[K.Locks]))
    
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Exists-Derived-From-Path"
        
        def trigger(xtra: Env.Xtra, fact: K.PathExists) = {
            val subpaths = fact.path match {
                case Path.Field(p: Path.Ref, _) => List(p)
                case Path.Cast(_, p) => List(p)
                case Path.Call(r: Path.Ref, _, args) => r :: args
                case Path.Call(Path.Static, _, args) => args
                case Path.Index(a, i) => List(a, i)
                case Path.Tuple(paths) => paths
                case _ => Nil
            }
            subpaths.map(K.PathExists)
        }
    })

    class TypeFactRule(
        val kind: Class[_ <: (K.Types with Fact.Forward)]
    ) extends Rule.Forward[Env.Xtra] {
        override def toString = "TypeFactRule(%s)".format(kind)
        val inputKinds = List(kind)
    
        override def derive(xtra: Env.Xtra, facts: List[Fact.Forward]): Iterable[Fact.Forward] = {
            val List(K.Types(l, r)) = facts
            List(K.TypeExists(l), K.TypeExists(r))
        }
    }
    addRule(new TypeFactRule(classOf[K.TypeEq]))
    addRule(new TypeFactRule(classOf[K.TypeUb]))

    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Exists-Derived-From-Type"
        
        private[this] def extract(arg: Type.Arg) = arg match {
            case Type.PathArg(_, _, _) => Nil
            case Type.TypeArg(_, _, ty) => List(K.TypeExists(ty))
        }
        
        def trigger(xtra: Env.Xtra, fact: K.TypeExists) = {
            fact.ty match {
                case Type.Class(_, args) => args.flatMap(extract)
                case Type.Tuple(types) => types.map(K.TypeExists)
                case Type.Member(path, _) => List(K.PathExists(path))
                case _ => Nil
            }
        }
    })
    
    // ______ Type Arguments ________________________________________________

    /*
    p : c[..., f rel q, ...]
    --------------------
    p.(c.f) rel q
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra] {
        override def toString = "typeArgs"
        
        def trigger(xtra: Env.Xtra, hasType: K.HasType) = {
            hasType match {
                case K.HasType(p, Type.Class(_, args)) => {
                    args.map {
                        case Type.PathArg(f, rel, q) => rel.toFact(Path.Field(p, f), q)
                        case Type.TypeArg(f, rel, t) => rel.toFact(Type.Member(p, f), t)
                    }
                }
                case _ => Nil
            }
        }
    })

    // ______ Equivalence Propagation _______________________________________
    //
    // The `Eq` relation has the nice property that any two paths that are
    // equivalent are also equivalent with respect to all other relations.
    // The following three templates implement this rule.  

    class PathEqualityRuleForward(
        otherKind: Fact.ForwardKind
    ) extends Rule.Forward[Env.Xtra] {
        override def toString = "PathEqualityRuleForward(%s)".format(otherKind)
        val inputKinds = List(classOf[K.PathEq], otherKind)
    
        def derive(xtra: Env.Xtra, facts: List[Fact.Forward]) = {
            val List(K.PathEq(p, q), K.Paths(l, r)) = facts
            val f = facts(1).asInstanceOf[K.ForwardPaths]
            List(
                p.is(l) toOption f.withPaths(q, r),
                p.is(r) toOption f.withPaths(l, q),
                q.is(l) toOption f.withPaths(p, r),
                q.is(r) toOption f.withPaths(l, p)
            ).flatten
        }
    }

    addRule(new PathEqualityRuleForward(classOf[K.PathEq]))
    addRule(new PathEqualityRuleForward(classOf[K.Hb]))
    addRule(new PathEqualityRuleForward(classOf[K.SubOf]))
    addRule(new PathEqualityRuleForward(classOf[K.InlineSubOf]))
    addRule(new PathEqualityRuleForward(classOf[K.Locks]))

    class PathEqualityRuleBackward(
        override val outputKind: Fact.BackwardKind
    ) extends Rule.Backward[Env.Xtra] {
        override def toString = "PathEqualityRuleBackward(%s)".format(outputKind)
        
        override def canInfer(recurse: Recurse[Env.Xtra], fact: Fact.Backward): Boolean = {
            val f @ K.Paths(p, q) = fact
            val eqToPs = recurse.queryRGivenL(p, classOf[K.PathEq])
            val eqToQs = recurse.queryLGivenR(classOf[K.PathEq], q)
            (eqToPs cross eqToQs).exists { case (eqToP, eqToQ) =>
                recurse.contains(f.withPaths(eqToP, eqToQ))
            }
        }
    }
    
    addRule(new PathEqualityRuleBackward(classOf[K.PermitsWr]))
    addRule(new PathEqualityRuleBackward(classOf[K.PermitsRd]))
    addRule(new PathEqualityRuleBackward(classOf[K.EnsuresFinal]))

    // ______ Path Equality _________________________________________________

    /*
    --------------------
    p == p
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "PathEq-Reflexive"
    
        def trigger(xtra: Env.Xtra, eq: K.PathExists): Iterable[Fact.Forward] = {
            val K.PathExists(p) = eq
            Some(K.PathEq(p, p))
        }
    })

    /*
    p == q
    --------------------
    q == p
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "PathEq-Symmetric"
    
        def trigger(xtra: Env.Xtra, eq: K.PathEq): Iterable[Fact.Forward] = {
            val K.PathEq(p, q) = eq
            Some(K.PathEq(q, p))
        }
    })

    /*
    --------------------
    (T)p == p
    etc

    Also handles the reflexive case (p == p).
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Path-Simplify"
    
        def trigger(xtra: Env.Xtra, fact: K.PathExists): Iterable[Fact.Forward] = {
            val path = fact.path
            path match {
                case Path.Cast(_, base) => Some(K.PathEq(path, base))
                case Path.Tuple(List(base)) => Some(K.PathEq(path, base))
                case Path.Index(Path.Tuple(paths), Path.Constant(index: Integer))
                if (index.intValue < paths.length) => Some(K.PathEq(path, paths(index.intValue)))
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
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Path-Induction-Path-Eq"
    
        def trigger(xtra: Env.Xtra, fact: K.PathExists, eq: K.PathEq): Iterable[Fact.Forward] = {
            val (K.PathExists(path), K.PathEq(p, q)) = (fact, eq)
            // XXX replaces ALL occurrences, ideally we'd replace all permutations
            Some(K.PathEq(path, Subst(p -> q).path(path)))
        }
    })

    // ______ Type Equality _________________________________________________

    /*
    t1 == t2
    --------------------
    t2 == t1
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Type-Eq-Reflexive"
    
        def trigger(xtra: Env.Xtra, eq: K.TypeEq): Iterable[Fact.Forward] = {
            val K.TypeEq(ty1, ty2) = eq
            Some(K.TypeEq(ty2, ty1))
        }
    })

    /*
    --------------------
    (t) == t
    etc
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Type-Eq-Inductive"
        
        def trigger(xtra: Env.Xtra, fact: K.TypeExists): Iterable[Fact.Forward] = {
            val K.TypeExists(ty) = fact
            List(K.TypeEq(ty, ty)) ++ (
                ty match {
                    case Type.Tuple(List(ty1)) => Some(K.TypeEq(ty, ty1))
                    case Type.Tuple(Nil) => Some(K.TypeEq(ty, Type.Void))
                    case _ => None
                }
            )
        }
    })

    /*
    p == q
    --------------------
    p.X == q.X
    where p/q are paths, X is a type variable
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Type-Induction-Path-Eq"
    
        def trigger(xtra: Env.Xtra, fact: K.TypeExists, factEq: K.PathEq): Iterable[Fact.Forward] = {
            val K.PathEq(p, q) = factEq
            val K.TypeExists(ty) = fact
            Some(K.TypeEq(ty, Subst(p -> q).ty(ty)))
        }
    })

    // ______ Path typing ___________________________________________________

    /*
    --------------------
    p : ty(p)
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Path-HasType"
    
        def trigger(xtra: Env.Xtra, fact: K.PathExists): Iterable[Fact.Forward] = {
            val path = fact.path
            xtra.symPath(path) match {
                case SPath.Typed(ty) => Some(K.HasType(path, ty))
                case _ => None
            }
        }
    })
    
    /*
    p : ty1
    ty1 ub ty2
    --------------------
    p : ty2
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Type-Induction-Path-Eq"
    
        def trigger(xtra: Env.Xtra, hasType: K.HasType, ub: K.TypeUb): Iterable[Fact.Forward] = {
            val K.HasType(p, ty1) = hasType
            ub match {
                case K.TypeUb(ty1(), ty2) => Some(K.HasType(p, ty2))
                case _ => None
            }
        }
    })
    
    /*
    class c1 extends c2[a1]
    c1[args] <: c2[a1 + a2]
    --------------------
    path : c2[a2]
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Extends"
    
        def trigger(xtra: Env.Xtra, ht: K.HasType): Iterable[Fact.Forward] = {
            ht match {
                case K.HasType(p, ty: Type.Class) =>
                    for(superTy <- xtra.superTypes(p, ty)) yield K.HasType(p, superTy)
                case _ => Nil
            }
        }
    })

    // ______ Bounding of type vars _________________________________________

    /*
    t1 ub t2
    t2 ub t1
    --------------------
    t1 eq t2
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Sub-Sup-Imply-Eq"
    
        def trigger(xtra: Env.Xtra, ub1: K.TypeUb, ub2: K.TypeUb): Iterable[Fact.Forward] = {
            val K.TypeUb(t1, t2) = ub1
            (ub2: @unchecked).matchAll(
                { case K.TypeUb(t2(), t1()) => K.TypeEq(t1, t2) }
            )
        }
    })

    /*
    t1 ub t2
    t2 eq t3
    --------------------
    t1 ub t3

    t1 ub t2
    t1 eq t3
    --------------------
    t3 ub t2
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Type-Ub-Eq-Prop"
    
        def trigger(xtra: Env.Xtra, ub: K.TypeUb, eq: K.TypeEq): Iterable[Fact.Forward] = {
            val K.TypeUb(t1, t2) = ub
            (eq: @unchecked).matchAll(
                { case K.TypeEq(t2(), t3) => K.TypeUb(t1, t3) },
                { case K.TypeEq(t1(), t3) => K.TypeUb(t3, t2) }
            )
        }
    })

    /*
    t1 ub t2
    t2 ub t3
    --------------------
    t1 ub t3
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Sub-Transitive"
    
        def trigger(xtra: Env.Xtra, bnd1: K.TypeUb, bnd2: K.TypeUb): Iterable[Fact.Forward] = {
            val K.TypeUb(t1, t2) = bnd1
            (bnd2: @unchecked).matchAll(
                { case K.TypeUb(t2(), t3) => K.TypeUb(t1, t3) }
            )
        }
    })

    /*
    t1 eq t2
    --------------------
    t1 ub t2
    t2 ub t1
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "Eq-Implies-Sub-Sup"
    
        def trigger(xtra: Env.Xtra, fact: K.TypeEq): Iterable[Fact.Forward] = {
            val K.TypeEq(t1, t2) = fact
            List(K.TypeUb(t1, t2), K.TypeUb(t2, t1))
        }
    })

    // ______ subOf, inlineSubOf, hb ________________________________________

    /*
    pc inlineSubOf pp
    --------------------
    pc subOf pp
    */

    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "InlineSubOf-Implies-SubOf"
    
        def trigger(xtra: Env.Xtra, inlineSubOf: K.InlineSubOf): Iterable[Fact.Forward] = {
            val K.InlineSubOf(pc, pp) = inlineSubOf
            Some(K.SubOf(pc, pp))
        }
    })

    /*
    pc subOf pp
    ------------
    pp.start hb pc.start
    pc.end hb pp.end
    */
    addRule(new Rule.ReflectiveForward[Env.Xtra]() {
        override def toString = "SubOf-Implies-HB"
    
        def trigger(xtra: Env.Xtra, subOf: K.SubOf): Iterable[Fact.Forward] = {
            import MethodId.{GetStart, GetEnd}
            val K.SubOf(pc, pp) = subOf
            List(
                K.Hb(pp.call(GetStart), pc.call(GetStart)),
                K.Hb(pc.call(GetEnd), pp.call(GetEnd))
            )
        }
    })

    // ______ permitsWr, permitsRd, ensuresFinal (built-in) _________________

    /*
    g permitsWr i
    --------------------
    g permitsRd i
    */
    addRule(new Rule.ReflectiveBackward[Env.Xtra]() {
        override def toString = "PermitsWr-Implies-PermitsRd"
    
        def trigger(recurse: Recurse[Env.Xtra], fact: K.PermitsRd): Boolean = {
            val K.PermitsRd(g, i) = fact
            recurse.contains(K.PermitsWr(g, i))
        }
    })

    /*
    g ensuresFinal i
    --------------------
    g permitsRd i
    */
    addRule(new Rule.ReflectiveBackward[Env.Xtra]() {
        override def toString = "EnsuresFinal-Implies-PermitsRd"
    
        def trigger(recurse: Recurse[Env.Xtra], fact: K.PermitsRd): Boolean = {
            val K.PermitsRd(g, i) = fact
            recurse.contains(K.EnsuresFinal(g, i))
        }
    })

    /*
    i subOf j
    g permitsRd j 
    --------------------
    g permitsRd i
    */
    addRule(new Rule.ReflectiveBackward[Env.Xtra]() {
        override def toString = "SubOf-PermitsRd"
    
        def trigger(recurse: Recurse[Env.Xtra], fact: K.PermitsRd): Boolean = {
            val K.PermitsRd(g, i) = fact
            recurse.queryRGivenL(i, classOf[K.SubOf]).exists { j =>
                recurse.contains(K.PermitsRd(g, j))                
            }
        }
    })

    /*
    i inlineSubOf j
    g permitsWr j 
    --------------------
    g permitsWr i
    */
    addRule(new Rule.ReflectiveBackward[Env.Xtra]() {
        override def toString = "InlineSubOf-PermitsWr"
    
        def trigger(recurse: Recurse[Env.Xtra], fact: K.PermitsWr): Boolean = {
            val K.PermitsWr(g, i) = fact
            recurse.queryRGivenL(i, classOf[K.InlineSubOf]).exists { j => 
                recurse.contains(K.PermitsWr(g, j))
            }
        }
    })

    /*
    --------------------
    final ensuresFinal i
    */
    addRule(new Rule.ReflectiveBackward[Env.Xtra]() {
        override def toString = "Final-EnsuresFinal"
    
        def trigger(recurse: Recurse[Env.Xtra], fact: K.EnsuresFinal): Boolean = {
            fact.left.is(Path.Final)
        }
    })

    /*
    --------------------
    path hasClass c
    */
    addRule(new Rule.ReflectiveBackward[Env.Xtra]() {
        override def toString = "Path-Has-Class"
    
        // TODO-- to make this more efficient we could add a bunch
        //        of special cases and avoid the need to fully compute
        //        symPath
    
        def trigger(recurse: Recurse[Env.Xtra], fact: K.HasClass): Boolean = {
            val global = recurse.xtra.global
            recurse.xtra.hasClass(
                recurse.xtra.symPath(fact.path),
                fact.cls,
                recurse.queryRGivenL(_, classOf[K.TypeUb])
            )
        }
    })
    
    // ______ permitsWr, permitsRd, ensuresFinal (mock) _____________________
    
    class MockRule(
        val outputKind: Fact.BackwardKind,
        val eval: ((intervals.guard.Guard, intervals.RoInterval) => RuntimeException)
    ) extends Rule.Backward[Env.Xtra] {
        override def toString = "Mock-" + outputKind.toString
        
        def canInfer(recurse: Recurse[Env.Xtra], fact: Fact.Backward): Boolean = {
            val K.GuardInter(guardPath, interPath) = fact.asInstanceOf[K.GuardInter]
            val menv = new mock.MockEnv(recurse)
            menv.tryMock(guardPath) match {
                case Some(guard: intervals.guard.StaticGuard) => {
                    menv.tryMock(interPath) match {
                        case Some(inter: intervals.RoInterval) => {
                            val err = eval(guard, inter)
                            (err == null)
                        }
                        case _ => false
                    }
                }
                case _ => false
            }
        }
    }
    
    addRule(new MockRule(classOf[K.PermitsWr], ((g, i) => g.checkWritable(i.getStart, i))))
    addRule(new MockRule(classOf[K.PermitsRd], ((g, i) => g.checkReadable(i.getStart, i))))
    addRule(new MockRule(classOf[K.EnsuresFinal], ((g, i) => g.ensuresFinal(i.getStart, i))))

}