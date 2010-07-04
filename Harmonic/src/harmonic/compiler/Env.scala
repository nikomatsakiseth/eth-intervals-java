package harmonic.compiler

import scala.util.parsing.input.NoPosition

import scala.collection.immutable.Set
import scala.collection.immutable.Queue
import scala.collection.mutable

import com.smallcultfollowing.lathos.model.Page
import com.smallcultfollowing.lathos.model.Context
import com.smallcultfollowing.lathos.model.PageContent
import com.smallcultfollowing.lathos.model.Output
import com.smallcultfollowing.lathos.model.{Util => LathosUtil}

import Util._
import Error.CanFail

object Env {
    def empty(global: Global) = Env(
        global       = global,
        thisTy       = Type.Top,
        optReturnTy  = None,
        locals       = Map(
            Name.FinalLocal -> global.finalSym
        ),
        pathRels     = Nil,
        typeRels     = Nil
    )
}

/** The environment is used during a type check but also in other phases
  * It stores information known by the compiler. */
case class Env(
    global: Global,
    
    /** Type of the this pointer */
    thisTy: Type.Class,
    
    /** Return type at current point, or 
      * None if returns not currently allowed */
    optReturnTy: Option[Type.Ref],
    
    /** In-scope local variables. */
    locals: Map[Name.LocalVar, VarSymbol.Local],
    
    /** Tuples describing relations between paths. */
    pathRels: List[Req.P],
    
    /** Tuples describing relations between type variables and other types. */
    typeRels: List[Req.T]
) extends Page {
    
    private[this] def log = global.closestLog
    
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

        out.subpage("Path Rels") {
            out.list(pathRels)
        }

        out.subpage("Type Rels") {
            out.list(typeRels)
        }
        
        out.endPage(this)
    }
    
    // ___ Transitive Closure Utility _______________________________________
    
    /** Base class that captures the basic pattern of computing
      * the transitive closure. */
    abstract class TransitiveCloser[T] {
        val visited = new mutable.HashSet[T]()
        
        def compute(item: T) = expand(Queue(item), Set())
        
        private[this] def expand(queue0: Queue[T], result: Set[T]): Set[T] = {
            if(queue0.isEmpty) result
            else {
                val (item, queue1) = queue0.dequeue
                val queue2 = 
                    if(visited(item)) queue1
                    else {
                        visited += item
                        successors(item).foldLeft(queue1)(_ enqueue _)
                    }
                expand(queue2, result + item)
            }
        }
        
        protected[this] def successors(item: T): Iterable[T]
    }
    
    // ___ Extending the Environment ________________________________________
    
    def plusLocalVar(sym: VarSymbol.Local) = copy(locals = locals + (sym.name -> sym))
    
    def plusLocalVars(syms: Iterable[VarSymbol.Local]) = syms.foldLeft(this)(_ plusLocalVar _)

    def plusThis(thisTy: Type.Class, sym: VarSymbol.Local) = plusLocalVar(sym).copy(thisTy = thisTy)
    
    def plusRel(rel: Req.Any) = rel match {
        case rel: Req.P => plusPathRel(rel)
        case rel: Req.T => plusTypeRel(rel)
    }
    
    def plusRels(rels: Iterable[Req.Any]) = rels.foldLeft(this)(_ plusRel _)
    
    def plusPathRel(rel: Req.P) = copy(pathRels = rel :: pathRels)

    def plusPathRels(rels: Iterable[Req.P]) = rels.foldLeft(this)(_ plusPathRel _)

    def plusTypeRel(rel: Req.T) = copy(typeRels = rel :: typeRels)
    
    def plusTypeRels(rels: Iterable[Req.T]) = rels.foldLeft(this)(_ plusTypeRel _)
    
    def withOptReturnTy(optReturnTy: Option[Type.Ref]) = copy(optReturnTy = optReturnTy)
    
    // ___ Querying the relations ___________________________________________
    
    def allRels = pathRels.view ++ typeRels.view
    
    private[this] def pathsRelatedBy(Rel: PcRel): List[(Path.Ref, Path.Ref)] = pathRels.flatMap { 
        case Req.P(p1, Rel, p2) => Some((p1, p2))
        case _ => None
    }
    
    private[this] def pathsRelatedBy(Rel: PcRel, P2: Path.Ref): List[Path.Ref] = pathRels.flatMap { 
        case Req.P(p1, Rel, P2) => Some(p1)
        case _ => None
    }
    
    private[this] def pathsRelatedBy(P1: Path.Ref, Rel: PcRel): List[Path.Ref] = pathRels.flatMap { 
        case Req.P(P1, Rel, p2) => Some(p2)
        case _ => None
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
        ownerTy: Type.Ref, 
        uName: Name.UnloweredMember
    )(
        func: (SymTab.MemberEntry => CanFail[R])
    ): CanFail[R] = {
        minimalUpperBoundType(ownerTy).firstRight[Error, R](Error.NoSuchMember(ownerTy, uName)) {
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
        ownerTy: Type.Ref, 
        uName: Name.UnloweredMember
    ): CanFail[Name.Member] = {
        lookupMember(ownerTy, uName) {
            case SymTab.Type(memberVar) => Right(memberVar)
            case entry => Left(Error.NotTypeVar(entry))
        }
    }

    private[this] def lookupField(
        ownerTy: Type.Ref, 
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
        ownerTy: Type.Ref, 
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
        ownerTy: Type.Ref,  
        name: Name.UnloweredMember,
        optExpTy: Option[Type.Ref]
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
        rcvrTy: Type.Ref, 
        methodName: Name.Method
    ): List[MethodSymbol] = {
        minimalUpperBoundType(rcvrTy).firstSome({ 
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
    
    // ___ Typed Paths ______________________________________________________   
    //
    // A Typed Path is simply a path where the symbols and type have been
    // determined.  A typed path may include error symbols if fields or local
    // variables found within are not defined.
    
    def typedPath(path: Path.Ref): Path.Typed = path match {
        case Path.Local(name: Name.LocalVar) => {
            val lvsym = locals.get(name).getOrElse(VarSymbol.errorLocal(name, None))
            Path.TypedLocal(lvsym)
        }
        
        case Path.Field(Path.Static, name) => {
            val csym = global.csym(name.className)
            val fsym = lookupFieldOrError(csym.toType, name, None)
            Path.TypedField(Path.Static, fsym)
        }
        
        case Path.Field(base: Path.Ref, name) => {
            val typedBase = typedPath(base)
            val sym = lookupFieldOrError(typedBase.ty, name, None)
            Path.TypedField(typedBase, sym)
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
        
        case Path.Call(_, _, _) => {
            throw new RuntimeException("TODO")
        }
    }
    
    def typeOfPath(path: Path.Ref) = typedPath(path).ty
    
    // ___ Equating Paths ___________________________________________________   
    //
    // Two paths are equatable if they will always refer to equal objects
    // at runtime.  We consider two objects equal if they are either
    // pointer-equal or if they are value objects with the same
    // constituents.  
    
    class Equater extends TransitiveCloser[Path.Ref] {
        private[this] def crossAll(paths: List[Path.Ref]): List[List[Path.Ref]] = {
            paths match {
                case path :: tl => {
                    val crossedTls = crossAll(tl)
                    compute(path).toList.flatMap { hd =>
                        crossedTls.map(hd :: _)
                    }
                }
                
                case List() => {
                    List()
                }
            }
        }
        
        protected[this] def successors(P1: Path.Ref): Iterable[Path.Ref] = {
            val byInduction = P1 match {
                case Path.Field(base: Path.Ref, name) => {
                    compute(base).map(Path.Field(_, name))
                }
                
                case Path.Cast(_, base) => {
                    Set(base)
                }
                
                case Path.Index(array, index) => {
                    (compute(array) cross compute(index)).map { case (a, i) =>
                        Path.Index(a, i)
                    }
                }
                
                case Path.Tuple(paths) => {
                    crossAll(paths).map(Path.Tuple)
                }
                
                case Path.Call(receiver: Path.Ref, methodName, args) => {
                    (compute(receiver) cross crossAll(args)).map { case (r, a) =>
                        Path.Call(r, methodName, a)
                    }
                }
                
                case Path.Field(Path.Static, _) 
                |   Path.Call(Path.Static, _, _)
                |   Path.Local(_)
                |   Path.Constant(_) => {
                    Set()                    
                }
            }
            
            val bySimplify = P1 match {
                case Path.Tuple(List(path)) => Some(path)
                
                case Path.Index(
                    Path.Tuple(paths), 
                    Path.Constant(index: java.lang.Integer)
                ) if index.intValue < paths.length => {
                    Some(paths(index.intValue))
                }
                
                case _ => None
            }
            
            val byRel = pathRels.flatMap {
                case Req.P(P1, PcEq, p2) => Some(p2)
                case Req.P(p2, PcEq, P1) => Some(p2)
                case _ => None
            }
            
            byInduction ++ bySimplify ++ byRel
        }
    }
    
    def equatable(path: Path.Ref) = new Equater().compute(path)
    def pathsAreEquatable(path1: Path.Ref, path2: Path.Ref) = equatable(path1) contains path2

    // ___ Relating Paths ___________________________________________________

    def pathsAreRelatable(rel: PcRel)(path1: Path.Ref, path2: Path.Ref): Boolean = {
        // TODO: Add code for handling permitsWr, permitsRd, and other derived relations!
        def search() = {
            val equatablePath1 = equatable(path1)
            val equatablePath2 = equatable(path2)
            pathRels.exists { case Req.P(p1, r, p2) =>
                (r == rel) && equatablePath1(p1) && equatablePath2(p2)
            }            
        }
        
        rel match {
            case PcEq => pathsAreEquatable(path1, path2)
            case PcEnsuresFinal if path1.is(Path.Final) => true
            case _ => search()
        }
    }
    
    // ___ Bounding Type Variables __________________________________________
    
    class Bounder(Rel: TcRel) extends TransitiveCloser[Type.Ref] {
        protected[this] def successors(ty: Type.Ref) = ty match {
            case tyVar: Type.Member => equatableTypeVars(tyVar) ++ typeVarBounds(tyVar)
            case Type.Tuple(List()) => List(Type.Void)   // () equivalent to Void  [Does this make sense?]
            case Type.Tuple(List(ty)) => List(ty)        // (ty) equivalent to ty
            case _ => Nil
        }
        
        // A type `p.v` is equatable with all types `q.v` where `p eq q`
        private[this] def equatableTypeVars(tyVar: Type.Member) = {
            equatable(tyVar.path).map { case q =>
                Type.Member(q, tyVar.typeVar)
            }
        }

        // A type `p.v` obtains bounds based on the type of `p`:
        private[this] def typeVarBounds(tyVar: Type.Member) = {
            val TyVarName = tyVar.typeVar
            
            // p has class type `pathTy` == `c[args]`:
            def boundsFromClassType(pathTy: Type.Class) = {
                // FIXME Add bounds declared in the environment:
                
                // Add bounds declared in the class `c`:
                val classBounds = List[Type.Ref]() // FIXME
                
                // Add bounds from type arguments `args` in `pathTy`:
                pathTy.typeArgs.foldLeft(classBounds) { 
                    case (l, Type.TypeArg(TyVarName, TcEq, ty)) => ty :: l
                    case (l, Type.TypeArg(TyVarName, Rel, ty)) => ty :: l
                    case (l, _) => l
                }
            }
            
            typeOfPath(tyVar.path) match {
                case pathTy: Type.Class => boundsFromClassType(pathTy)
                case pathTy: Type.Member => {
                    compute(pathTy).toList.flatMap {
                        case tyClass: Type.Class => boundsFromClassType(tyClass)
                        case _ => Nil
                    }
                }
                case Type.Tuple(_) => Nil
                case Type.Null => Nil
            }
        }
    }
    
    /** Returns a set of types that are exactly equivalent to `ty`. */
    def equateVars(ty: Type.Ref) = new Bounder(TcEq).compute(ty)

    /** Returns a set of types that are upper bounds for `ty` 
      * (i.e., supertypes of `ty`).  This function is intended to expand
      * type variables.  It does not return supertypes of class types. */
    def upperBoundVars(ty: Type.Ref) = new Bounder(TcSub).compute(ty)
    
    /** Returns a set of types that are lower bounds for `ty` 
      * (i.e., subtypes of `ty`). This function is intended to expand
      * type variables.  It does not return supertypes of class types. */
    def lowerBoundVars(ty: Type.Ref) = new Bounder(TcSup).compute(ty)
    
    /** Returns a minimal set of upper-bounds for `ty`. "Minimal"
      * means that we remove redundant class types; i.e., if 
      * references to classes C and D are both in the list, and
      * C extends D, then D will be removed. */
    def minimalUpperBoundType(ty: Type.Ref) = {
        val bnds = upperBoundVars(ty)
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
    
    def minimalUpperBoundClassTys(ty: Type.Ref) = {
        minimalUpperBoundType(ty).flatMap {
            case clsTy: Type.Class => Some(clsTy)
            case _ => None
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
    
    private[this] def mutualUpperBoundVar(varTy: Type.Member, otherTy: Type.Ref) = {
        val ub = minimalUpperBoundType(varTy)
        if(ub.contains(otherTy)) otherTy
        else ub.firstSome({
            case clsTy: Type.Class => Some(mutualUpperBound(clsTy, otherTy))
            case _ => None
        }).getOrElse(Type.Top)
    }
    
    private[this] def mutualUpperBoundUnmatchedTuple(tys: List[Type.Ref], otherTy: Type.Ref) = {
        val boundTy = mutualUpperBoundOfList(tys)
        mutualUpperBound((Type.arrayExtends(boundTy), otherTy))
    }
    
    def mutualUpperBoundOfList(tys: List[Type.Ref]): Type.Ref = {
        tys match {
            case List() => Type.Top
            case List(ty) => ty
            case tys => tys.reduceLeft { (a, b) => mutualUpperBound((a, b)) }
        }
    }
    
    /** Given a pair of types, returns a new type that is a supertype
      * of both.  Tries to pick a precise type when possible. */
    def mutualUpperBound(pair: (Type.Ref, Type.Ref)): Type.Ref = {
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
    
    private[this] def isSuitableArgumentBounded(ty_val: Type.Ref, ty_pat: Type.Ref): Boolean = {
        (ty_val, ty_pat) match {
            case (Type.Class(name_val, _), Type.Class(name_pat, _)) => {
                val sym_val = global.csym(name_val)
                val sym_pat = global.csym(name_pat)
                sym_val.isSubclass(sym_pat)
            }
            
            case (Type.Member(path_val, tvar_val), Type.Member(path_pat, tvar_pat)) if tvar_val == tvar_pat =>
                pathsAreEquatable(path_val, path_pat)
                
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
    
    def isSuitableArgument(ty_val: Type.Ref, ty_pat: Type.Ref): Boolean = {
        (upperBoundVars(ty_val) cross lowerBoundVars(ty_pat)).exists {
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

    private[this] def typesAreEquatable1(pair: (Type.Ref, Type.Ref)): Boolean = {
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
    
    def typesAreEquatable(ty1: Type.Ref, ty2: Type.Ref): Boolean = {
        (ty1 == ty2) || (equateVars(ty1) cross equateVars(ty2)).exists(typesAreEquatable1)
    }
    
    private[this] def typeEquatableWith(ty1: Type.Ref)(ty2: Type.Ref): Boolean = {
        typesAreEquatable(ty1, ty2)
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
            typesAreEquatable(pp_sub.ty, subst.ty(pp_sup.ty))
        }
    }
    
//    def isSubclass(ty_sub: Type.Ref, ty_sup: Type.Ref): Boolean = {
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
//    def matchesByClass(pattern: Pattern.Ref, ty: Type.Ref): Boolean = {
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
        log.indent(this, ".isSatisfiedForPath(", path, ")(", arg, ")") {
            arg match {
                case Type.PathArg(name, rel, path2) => {
                    val extPath = Path.Field(path.toPath, name)
                    pathsAreRelatable(rel)(extPath, path2)
                }
                case Type.TypeArg(name, rel, ty) => {
                    val extTy = Type.Member(path.toPath, name)
                    new Bounder(rel).compute(extTy).exists(
                        typeEquatableWith(ty)
                    )
                }
            }            
        }
    }
    
    def pathHasType(path: Path.Typed, ty: Type.Ref): Boolean = {
        log.indent(this, ".pathHasType(", path, ", ", ty, ")") {
            val ubSubTys = upperBoundVars(path.ty)
            val lbSuperTys = lowerBoundVars(ty)
            (ubSubTys cross lbSuperTys).exists {
                case (Type.Null, _) => {
                    true
                }
            
                case (t1 @ Type.Member(path1, v1), t2 @ Type.Member(path2, v2)) => {
                    log.indent("Member types: ", t1, " and ", t2) {
                        (v1 == v2) && pathsAreEquatable(path1, path2)                    
                    }
                }
            
                case (t1 @ Type.Class(subName, _), t2 @ Type.Class(supName, supArgs)) => {
                    log.indent("Class types: ", t1, " and ", t2) {
                        val subCsym = global.csym(subName)
                        val supCsym = global.csym(supName)
                        subCsym.isSubclass(supCsym) && supArgs.forall(isSatisfiedForPath(path))
                    }
                }
            
                case (_, _) => {
                    false
                }
            }
        }
    }
    
    def isSubtype(subTy: Type.Ref, supTy: Type.Ref): Boolean = {
        val tempSym = new VarSymbol.Local(
            NoPosition, 
            Modifier.Set.empty, 
            global.freshLocalName,
            subTy
        )
        plusLocalVar(tempSym).pathHasType(tempSym.toTypedPath, supTy)
    }
  
    // ___ Relations ________________________________________________________
    
    def pathRelHolds(rel: Req.P): Boolean = {
        pathsAreRelatable(rel.rel)(rel.left, rel.right)
    }

    def typeRelHolds(rel: Req.T): Boolean = rel match {
        case Req.T(l, TcEq, r) => typesAreEquatable(l, r)
        case Req.T(l, TcSub, r) => isSubtype(l, r)
        case Req.T(l, TcSup, r) => isSubtype(r, l)
    }
    
    def relHolds(rel: Req.Any): Boolean = rel match {
        case rel: Req.P => pathRelHolds(rel)
        case rel: Req.T => typeRelHolds(rel)
    }

    // ___ Path is final by _________________________________________________
    //
    // Determines whether a given path has reached its final value by
    // the given interval.  The path `inter` is assumed to be final.
    
    def relIsFinalBy(rel: Req.Any, inter: Path.Typed) = {
        rel match {
            case rel: Req.P => pathIsFinalBy(typedPath(rel.left), inter) && pathIsFinalBy(typedPath(rel.right), inter)
            case rel: Req.T => typeIsFinalBy(rel.left, inter) && typeIsFinalBy(rel.right, inter)
        }
    }
    
    def typeIsFinalBy(ty: Type.Ref, inter: Path.Typed): Boolean = {
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
                    pathsAreRelatable(PcEnsuresFinal)(guardPath.toPath, inter.toPath)
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
                    pathsAreRelatable(PcEnsuresFinal)(guardPath.toPath, inter.toPath)
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
                    pathsAreRelatable(PcEnsuresFinal)(guardPath, inter.toPath)
                }
            }
            
        }
    }
    
  
}