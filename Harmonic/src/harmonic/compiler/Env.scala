package harmonic.compiler

import scala.util.parsing.input.NoPosition

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import scala.collection.immutable.Queue
import scala.collection.mutable

import com.smallcultfollowing.lathos.Page
import com.smallcultfollowing.lathos.Context
import com.smallcultfollowing.lathos.PageContent
import com.smallcultfollowing.lathos.Output
import com.smallcultfollowing.lathos.Lathos

import harmonic.compiler.inference.FactSet

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
    def empty(global: Global) = {
        val xtra = Env.Xtra(global, Map(Name.FinalLocal -> global.finalSym))
        Env(
            global      = global,
            thisTy      = Type.Top,
            optReturnTy = None,
            factSet     = global.network.emptyFactSet(xtra)
        )
    }
    
    case class Xtra(
        global: Global,
        locals: Map[Name.LocalVar, VarSymbol.Local]
    ) {

        def plusLocalVar(sym: VarSymbol.Local) = 
            copy(locals = locals + (sym.name -> sym))

        def typedOwner(owner: Path.UntypedOwner): Path.TypedOwner = owner match {
            case Path.Static => Path.Static
            case owner: Path.Ref => typedPath(owner)
        }

        def typedPath(path: Path.Ref): Path.Typed = Lathos.context.indent(this, ".typedPath(", path, ")") {
            path match {
                case Path.Local(name: Name.LocalVar) => {
                    val lvsym = locals.get(name).getOrElse(VarSymbol.errorLocal(name, None))
                    Lathos.context.log("Local variable: ", name, " symbol: ", lvsym)
                    Path.TypedLocal(lvsym)
                }

                case Path.Field(owner, name) => {
                    val fsym = global.fieldSymbol(name).getOrElse {
                        // TODO --- report invalid method ids at the site of use.
                        Error.NoSuchField(name).report(
                            global, 
                            InterPosition.forClassNamed(name.className)
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
        }

        def typeOfPath(path: Path.Ref) = typedPath(path).ty

        def superTypes(ty: Type.Class) = Lathos.context.embeddedIndent("superTypes(", ty, ")") {
            val Type.Class(nm, args) = ty
            val csym = global.csym(nm)
            csym.superClassNames.map { c2 =>
                // TODO: Add in any applicable constraints defined in c1!!
                // TODO: Filter out inapplicable arguments from ty (won't hurt though)
                Type.Class(c2, args)
            }
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
    
    /** Known facts */
    factSet: inference.FactSet[Env.Xtra]
) extends DebugPage {
    override def toString = getId
    
    // ___ Extending the Environment ________________________________________
    
    def plusLocalVar(sym: VarSymbol.Local): Env = 
        copy(factSet = factSet.plusXtra(xtra.plusLocalVar(sym)))
        
    def plusThis(thisTy: Type.Class, sym: VarSymbol.Local): Env = 
        plusLocalVar(sym).copy(thisTy = thisTy)
        
    def withOptReturnTy(optReturnTy: Option[Type]): Env = 
        copy(optReturnTy = optReturnTy)
        
    def plusFact(fact: inference.Fact): Env = 
        plusFacts(Some(fact))
        
    def plusFacts(facts: Iterable[inference.Fact]): Env =
        copy(factSet = factSet.plusFacts(facts, xtra))
        
    def plusFactSet(factSet: inference.FactSet[Env.Xtra]): Env =
        copy(factSet = factSet.plusFactSet(factSet, xtra))

    // ___ Simple queries ___________________________________________________
    
    private[this] def xtra = factSet.xtra
    
    def locals = xtra.locals
    
    def factHolds(fact: inference.Fact): Boolean = {
        Lathos.context.indent(this, ".factHolds(", fact, ")? Consulting ", factSet) {
            factSet.contains(fact)
        }
    }
    
    def typesAreEquatable(ty1: Type, ty2: Type) = factHolds(K.TypeEq(ty1, ty2))
    def pathsAreEquatable(p1: Path.Ref, p2: Path.Ref) = factHolds(K.PathEq(p1, p2))
    def typedPath(path: Path.Ref) = xtra.typedPath(path)
    def typeOfPath(path: Path.Ref) = typedPath(path).ty
    def ensuresFinal(guard: Path.Ref, inter: Path.Ref) = factHolds(K.EnsuresFinal(guard, inter))
    def upperBounds(ty: Type) = {
        Lathos.context.indent(this, ".upperBounds(", ty, ")? Consulting ", factSet) {
            factSet.queryRGivenL[Type, Type, K.TypeUb](classOf[K.TypeUb], ty)
        }
    }
    def lowerBounds(ty: Type) = {
        Lathos.context.indent(this, ".lowerBounds(", ty, ") Consulting ", factSet) {
            factSet.queryLGivenR[Type, Type, K.TypeUb](classOf[K.TypeUb], ty)
        }
    }

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
            case Type.Class(className, _) => 
                val mro = global.csym(className).mro
                mro.firstSome { csym =>
                    lookupInstanceMethodsDefinedOnClass(csym.name, methodName) match {
                        case List() => None
                        case msyms => Some(msyms)
                    }                    
                }
            case _ => None
        }).getOrElse(List())
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
    
    private[this] def isSuitableArgument1(ty_val: Type, ty_pat: Type): Boolean = {
        Lathos.context.embeddedIndent("isSuitableArgument1(", ty_val, ", ", ty_pat, ")") {
            (ty_val, ty_pat) match {
                case (Type.Class(name_val, _), Type.Class(name_pat, _)) => {
                    val sym_val = global.csym(name_val)
                    val sym_pat = global.csym(name_pat)
                    sym_val.isSubclass(sym_pat)
                }

                case (Type.Member(path_val, tvar_val), Type.Member(path_pat, tvar_pat)) if tvar_val == tvar_pat =>
                    pathsAreEquatable(path_val, path_pat)

                case (Type.Tuple(List(ty)), _) =>
                    isSuitableArgument1(ty, ty_pat)

                case (_, Type.Tuple(List(ty))) =>
                    isSuitableArgument1(ty_val, ty)

                case (Type.Tuple(tys_val), Type.Tuple(tys_pat)) if sameLength(tys_val, tys_pat) =>
                    tys_val.zip(tys_pat).forall { case (v, p) => isSuitableArgument(v, p) }

                case (Type.Null, _) => 
                    true

                case _ =>
                    false
            }            
        }
    }
    
    def isSuitableArgument(ty_val: Type, ty_pat: Type): Boolean = {
        Lathos.context.indent("isSuitableArgument(", ty_val, ", ", ty_pat, ")") {
            (upperBounds(ty_val) cross lowerBounds(ty_pat)).exists {
                case (u, l) => isSuitableArgument1(u, l)
            }            
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
    ) = Lathos.context.indent("overrides(", msig_sub, ", ", msig_sup, ")") {
        val pps_sub = msig_sub.parameterPatterns
        val pps_sup = msig_sup.parameterPatterns
        sameLength(pps_sub, pps_sup) && {
            val subst = pps_sub.zip(pps_sup).foldLeft(Subst.empty)(addOverrideSubst)
            pps_sub.zip(pps_sup).forall { case (pp_sub, pp_sup) => 
                typesAreEquatable(pp_sub.ty, subst.ty(pp_sup.ty))
            }            
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

    // ___ Path is final by _________________________________________________
    //
    // Determines whether a given path has reached its final value by
    // the given interval.  The path `inter` is assumed to be final.
    
    def factIsFinalBy(fact: inference.Fact, inter: Path.Typed) = {
        fact match {
            case K.Paths(l, r) => pathIsFinalBy(typedPath(l), inter) && pathIsFinalBy(typedPath(r), inter)
            case K.Types(l, r) => typeIsFinalBy(l, inter) && typeIsFinalBy(r, inter)
            case K.HasType(p, t) => pathIsFinalBy(typedPath(p), inter) && typeIsFinalBy(t, inter)
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
                    ensuresFinal(guardPath.toPath, inter.toPath)
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
                    ensuresFinal(guardPath.toPath, inter.toPath)
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
                    ensuresFinal(guardPath, inter.toPath)
                }
            }
            
        }
    }

    // ______ Assignable and subtyping ______________________________________
    // 
    // You might think that Assignable would be implemented in terms of
    // subtyping but this is not the case!  In fact it is the opposite.
    //
    // This is because we may know information about the path being assigned 
    // from that is not encoded in the path's type.  In that case, these
    // extra facts may be useful to showing that path is assignable to ty.
    
    def isSubtype(ty1: Type, ty2: Type): Boolean = {
        // To determine if args1 => args2:
        // * Gin up a fake local variable x of type c[args1].
        // * Check whether x → c[args2].
    
        val fresh = new VarSymbol.Local(
            pos = NoPosition,
            modifiers = Modifier.Set.empty,
            name = global.freshLocalName,
            ty = ty1
        )

        plusLocalVar(fresh).isAssignable(fresh.toPath, ty2)
    }
    
    /** A path `p` is assignable to a lvalue of type `ty` if 
      * (1) `p.ty` is of the right class and satisfies all constraints on `ty`; or,
      * (2) `p.ty` is upper-bounded by `ty` */
    def isAssignable(path: Path.Ref, ty: Type): Boolean = {
        // True if `arg` is satisfied relative to `path`:
        def isSatisfiedForPath(arg: Type.Arg): Boolean = {
            arg match {
                // For paths we always rely on the factSet:
                case Type.PathArg(name, rel, path2) => {
                    val extPath = Path.Field(path, name)
                    factHolds(rel.toFact(extPath, path2))
                }
                
                // But subtyping works differently:
                case Type.TypeArg(name, TcSub, ty) => {
                    val extTy = Type.Member(path, name)
                    isSubtype(extTy, ty)
                }
                case Type.TypeArg(name, TcSup, ty) => {
                    val extTy = Type.Member(path, name)
                    isSubtype(ty, extTy)
                }
                case Type.TypeArg(name, TcEq, ty) => {
                    val extTy = Type.Member(path, name)
                    isSubtype(extTy, ty) && isSubtype(ty, extTy)
                }
            }            
        }

        // Find upper bounds of path's type and lower bounds of 
        // lvalue's type, then check to see if they intersect:
        val pathTy = typeOfPath(path)
        val ubPathTys = upperBounds(pathTy)
        val lbLvalueTys = lowerBounds(ty)
        (ubPathTys cross lbLvalueTys).exists {
            case (Type.Null, _) =>
                true

            case (Type.Class(n1, _), Type.Class(n2, args)) if n1.is(n2) =>
                args.forall(isSatisfiedForPath)

            case (t1, t2) =>
                t1.is(t2)
        }
    }
  
}