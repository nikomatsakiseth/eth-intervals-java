package harmonic.compiler

import scala.util.parsing.input.NoPosition

import com.smallcultfollowing.lathos

import harmonic.compiler.inference.Network
import harmonic.compiler.inference.Fact
import harmonic.compiler.inference.Rule
import harmonic.compiler.inference.Recurse

import Util._

object HarmonicRulesNetwork
{
    trait Xtra {
        val locals: Map[Name.LocalVar, VarSymbol.Local]
        val global: Global
        def typeOfPath(path: Path.Ref): Type
        def superTypes(ty: Type.Class): List[Type.Class]
    }
}

/** Defines the inference rules for the Harmonic type system. 
  * The facts which we operate over are defined in the K module. */
class HarmonicRulesNetwork(server: lathos.model.LathosServer)
extends Network[HarmonicRulesNetwork.Xtra](server)
{
    import HarmonicRulesNetwork.Xtra
    
    // ______ Path and Type facts ___________________________________________
    //
    // These are kind of a workaround that allows us to avoid using backwards
    // propagation and its inherent complications with transitive relations.
    //
    // Basically we add a Path or Type fact for every referenced type.  
    // This is then used to trigger various forward rules that handle 
    // "0-predicate" rules.

    class PathFactRule(
        val kind: Class[_ <: K.Paths]
    ) extends Rule.Forward[Xtra] {
        val inputKinds = List(kind)
    
        def derive(xtra: Xtra, facts: List[Fact]) = {
            val List(K.Paths(l, r)) = facts
            List(K.PathExists(l), K.PathExists(r))
        }
    }
    addRule(new PathFactRule(classOf[K.PathEq]))
    addRule(new PathFactRule(classOf[K.Hb]))
    addRule(new PathFactRule(classOf[K.SubOf]))
    addRule(new PathFactRule(classOf[K.InlineSubOf]))
    addRule(new PathFactRule(classOf[K.Locks]))
    
    addRule(new Rule.Forward[Xtra]() {
        def trigger(xtra: Xtra, fact: K.PathExists) = {
            val subpaths = fact.path match {
                case Path.Field(p: Path.Ref, _) => List(p)
                case Path.Cast(_, p) => List(p)
                case Path.Call(r, _, args) => r :: args
                case Path.Index(a, i) => List(a, i)
                case Path.Tuple(paths) => paths
                case _ => Nil
            }
            subpaths.map(K.PathExists)
        }
    })

    class TypeFactRule(
        val kind: Class[_ <: K.Types]
    ) extends Rule.Forward[Xtra] {
        val inputKinds = List(kind)
    
        def derive(xtra: Xtra, facts: List[Fact]) = {
            val List(K.Types(l, r)) = facts
            List(K.TypeExists(l), K.TypeExists(r))
        }
    }
    addRule(new TypeFactRule(classOf[K.TypeEq]))
    addRule(new TypeFactRule(classOf[K.TypeUb]))

    addRule(new Rule.Forward[Xtra]() {
        private[this] def extract(arg: Type.Arg) = arg match {
            case Type.PathArg(_, _, _) => None
            case Type.TypeArg(_, _, ty) => Some(ty)
        }
        
        def trigger(xtra: Xtra, fact: K.TypeExists) = {
            val subtypes = fact.ty match {
                case Type.Class(_, args) => args.flatMap(extract)
                case Type.Tuple(types) => types
                case _ => Nil
            }
            subtypes.map(K.TypeExists)
        }
    })
    
    // ______ Type Arguments ________________________________________________

    /*
    p : c[..., f rel q, ...]
    --------------------
    p.(c.f) rel q
    */
    addRule(new Rule.ReflectiveForward[Xtra] {
        def trigger(xtra: Xtra, fact: K.PathExists, hasType: K.HasType) = {
            fact.path match {
                case Path.Field(p, _) => {
                    hasType match {
                        case K.HasType(p(), Type.Class(_, args)) => {
                            args.map {
                                case Type.PathArg(f, rel, q) => rel.toFact(Path.Field(p, f), q)
                                case Type.TypeArg(f, rel, t) => rel.toFact(Type.Member(p, f), t)
                            }
                        }
                        case _ => Nil
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
    ) extends Rule.Forward[Xtra] {
        val inputKinds = List(classOf[K.PathEq], otherKind)
    
        def derive(xtra: Xtra, facts: List[Fact]) = {
            val List(K.PathEq(p, q), f @ K.Paths(l, r)) = facts
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
        outputKind: Fact.BackwardKind
    ) extends Rule.Backward[Xtra] {
        def derive(recurse: Recurse[Xtra], fact: Fact.Backward) = {
            val f @ K.Paths(p, q) = fact
            val eqs = recurse.allFactsOfKind(classOf[K.PathEq])
            val eqToPs = eqs.flatMap {
                case K.PathEq(p(), r) => Some(r)
                case _ => None
            }
            val eqToQs = eqs.flatMap {
                case K.PathEq(r, q()) => Some(r)
                case _ => None 
            }
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "PathEq-Reflexive"
    
        def trigger(xtra: Xtra, eq: K.PathExists) = {
            val K.PathExists(p) = eq
            Some(K.PathEq(p, p))
        }
    })

    /*
    p == q
    --------------------
    q == p
    */
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "PathEq-Symmetric"
    
        def trigger(xtra: Xtra, eq: K.PathEq) = {
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Path-Simplify"
    
        def trigger(xtra: Xtra, fact: K.PathExists) = {
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Path-Induction-Path-Eq"
    
        def trigger(xtra: Xtra, fact: K.PathExists, eq: K.PathEq) = {
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Type-Eq-Reflexive"
    
        def trigger(xtra: Xtra, eq: K.TypeEq) = {
            val K.TypeEq(ty1, ty2) = eq
            Some(K.TypeEq(ty2, ty1))
        }
    })

    /*
    --------------------
    (t) == t
    etc

    Also handles the reflexive case (t == t).
    */
    addRule(new Rule.ReflectiveForward[Xtra]() {
        def trigger(xtra: Xtra, fact: K.TypeExists) = {
            val K.TypeExists(ty) = fact
            ty.matchAll(
                { case _ => K.TypeEq(ty, ty) },
                { case Type.Tuple(List(ty1)) => K.TypeEq(ty, ty1) },
                { case Type.Tuple(Nil) => K.TypeEq(ty, Type.Void) }
            )
        }
    })

    /*
    p == q
    --------------------
    p.X == q.X
    where p/q are paths, X is a type variable
    */
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Type-Induction-Path-Eq"
    
        def trigger(xtra: Xtra, fact: K.TypeExists, factEq: K.PathEq) = {
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Path-HasType"
    
        def trigger(xtra: Xtra, fact: K.PathExists) = {
            val path = fact.path
            val ty = xtra.typeOfPath(path)
            Some(K.HasType(path, ty))
        }
    })
    
    /*
    p : ty1
    ty1 ub ty2
    --------------------
    p : ty2
    */
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Type-Induction-Path-Eq"
    
        def trigger(xtra: Xtra, hasType: K.HasType, ub: K.TypeUb) = {
            val K.HasType(p, ty1) = hasType
            ub match {
                case K.TypeUb(ty1(), ty2) => Some(K.HasType(p, ty2))
                case _ => None
            }
        }
    })
    
    // ______ Bounding ______________________________________________________

    /*
    class c1 extends c2
    --------------------
    c1[args] <: c2[args]
    */
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Extends"
    
        def trigger(xtra: Xtra, sub: K.TypeExists) = {
            sub.ty match {
                case ty: Type.Class => xtra.superTypes(ty).map(K.TypeUb(ty, _))
                case _ => Nil
            }
        }
    })

    /*
    t1 ub t2
    t2 ub t1
    --------------------
    t1 eq t2
    */
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Sub-Sup-Imply-Eq"
    
        def trigger(xtra: Xtra, ub1: K.TypeUb, ub2: K.TypeUb) = {
            val K.TypeUb(t1, t2) = ub1
            ub2.matchAll(
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = ""
    
        def trigger(xtra: Xtra, ub: K.TypeUb, eq: K.TypeEq) = {
            val K.TypeUb(t1, t2) = ub
            eq.matchAll(
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Sub-Transitive"
    
        def trigger(xtra: Xtra, bnd1: K.TypeUb, bnd2: K.TypeUb) = {
            val K.TypeUb(t1, t2) = bnd1
            bnd2.matchAll(
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "Eq-Implies-Sub-Sup"
    
        def trigger(xtra: Xtra, fact: K.TypeEq) = {
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

    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "InlineSubOf-Implies-SubOf"
    
        def trigger(xtra: Xtra, inlineSubOf: K.InlineSubOf) = {
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
    addRule(new Rule.ReflectiveForward[Xtra]() {
        override def toString = "SubOf-Implies-HB"
    
        def trigger(xtra: Xtra, subOf: K.SubOf) = {
            import MethodId.{GetStart, GetEnd}
            val K.SubOf(pc, pp) = subOf
            List(
                K.Hb(pp.call(GetStart), pc.call(GetStart)),
                K.Hb(pc.call(GetEnd), pp.call(GetEnd))
            )
        }
    })

    // ______ permitsWr, permitsRd, ensuresFinal ____________________________

    /*
    g permitsWr i
    --------------------
    g permitsRd i
    */
    addRule(new Rule.ReflectiveBackward[Xtra]() {
        override def toString = "PermitsWr-Implies-PermitsRd"
    
        def trigger(recurse: Recurse[Xtra], fact: K.PermitsRd) = {
            val K.PermitsRd(g, i) = fact
            recurse.contains(K.PermitsWr(g, i))
        }
    })

    /*
    g ensuresFinal i
    --------------------
    g permitsRd i
    */
    addRule(new Rule.ReflectiveBackward[Xtra]() {
        override def toString = "EnsuresFinal-Implies-PermitsRd"
    
        def trigger(recurse: Recurse[Xtra], fact: K.PermitsRd) = {
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
    addRule(new Rule.ReflectiveBackward[Xtra]() {
        override def toString = "SubOf-PermitsRd"
    
        def trigger(recurse: Recurse[Xtra], fact: K.PermitsRd) = {
            val K.PermitsRd(g, i) = fact
            recurse.allFactsOfKind(classOf[K.SubOf]).exists {
                case K.SubOf(i(), j) => {
                    recurse.contains(K.PermitsRd(g, j))                
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
    addRule(new Rule.ReflectiveBackward[Xtra]() {
        override def toString = "InlineSubOf-PermitsWr"
    
        def trigger(recurse: Recurse[Xtra], fact: K.PermitsWr) = {
            val K.PermitsWr(g, i) = fact
            recurse.allFactsOfKind(classOf[K.InlineSubOf]).exists {
                case K.InlineSubOf(i(), j) => {
                    recurse.contains(K.PermitsWr(g, j))
                }
                case _ => false
            }
        }
    })

    /*
    --------------------
    final ensuresFinal i
    */
    addRule(new Rule.ReflectiveBackward[Xtra]() {
        override def toString = "Final-EnsuresFinal"
    
        def trigger(recurse: Recurse[Xtra], fact: K.EnsuresFinal) = {
            fact.left.is(Path.Final)
        }
    })
}