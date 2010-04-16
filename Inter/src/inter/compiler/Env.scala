package inter.compiler

import scala.collection.immutable.Set
import scala.collection.immutable.Queue
import scala.collection.mutable

import Util._

object Env {
    def empty(state: CompilationState) = Env(
        state = state,
        locals = Map(),
        pathRels = Nil,
        typeRels = Nil
    )
    
}

/** The environment is used during a type check but also in other phases
  * It stores information known by the compiler. */
case class Env(
    state: CompilationState,
    
    /** In-scope local variables. */
    locals: Map[Name.Var, Symbol.Var],
    
    /** Tuples describing relations between paths. */
    pathRels: List[(Path.Ref, PcRel, Path.Ref)],
    
    /** Tuples describing relations between type variables and other types. */
    typeRels: List[(Type.Var, TcRel, Type.Ref)]
) {
    
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
    
    def +(sym: Symbol.Var) = copy(locals = locals + (sym.name -> sym))
    def ++(syms: Iterable[Symbol.Var]) = syms.foldLeft(this)(_ + _)
    
    // ___ Querying the relations ___________________________________________
    
    private[this] def pathsRelatedBy(Rel: PcRel): List[(Path.Ref, Path.Ref)] = pathRels.flatMap { 
        case (p1, Rel, p2) => Some((p1, p2))
        case _ => None
    }
    
    private[this] def pathsRelatedBy(Rel: PcRel, P2: Path.Ref): List[Path.Ref] = pathRels.flatMap { 
        case (p1, Rel, P2) => Some(p1)
        case _ => None
    }
    
    private[this] def pathsRelatedBy(P1: Path.Ref, Rel: PcRel): List[Path.Ref] = pathRels.flatMap { 
        case (P1, Rel, p2) => Some(p2)
        case _ => None
    }
    
    // ___ Looking up fields and methods ____________________________________
    //
    // Note: this can trigger lowering to occur!  
    
    def localIsDefined(name: Name.Var) = 
        locals.isDefinedAt(name)
    
    def lookupLocal(name: Name.Var) = 
        locals.get(name)
    
    def lookupLocalOrError(name: Name.Var, optExpTy: Option[Type.Ref]) = 
        locals.get(name).getOrElse(Symbol.errorVar(name, optExpTy))
        
    def lookupThis = locals(Name.This)
    
    def lookupField(
        rcvrTy: Type.Ref, 
        name: Name.Var
    ): Option[Symbol.Var] = {
        rcvrTy match {
            case Type.Class(className, _) => {
                val csym = state.symtab.classes(className)
                csym.fieldNamed(state)(name)
            }
            
            case tyVar: Type.Var => {
                upperBoundType(tyVar).firstSome(lookupField(_, name))
            }
            
            case _ => None
        }
    }
    
    def lookupFieldOrError(
        rcvrTy: Type.Ref,  
        name: Name.Var,
        optExpTy: Option[Type.Ref]
    ) = {
        lookupField(rcvrTy, name).getOrElse {
            Symbol.errorVar(name, optExpTy)
        }
    }    
    
    def lookupNonintrinsicMethods(
        rcvrTy: Type.Ref, 
        name: Name.Method
    ): List[Symbol.Method] = {
        rcvrTy match {
            case Type.Class(className, _) => {
                val csym = state.symtab.classes(className)
                csym.methodsNamed(state)(name)
            }
            
            case tyVar: Type.Var => {
                upperBoundType(tyVar).toList.flatMap(lookupNonintrinsicMethods(_, name))
            }
            
            case _ => List()
        }
    }

    def lookupMethods(
        rcvrTy: Type.Ref, 
        name: Name.Method
    ): List[Symbol.Method] = {
        state.lookupIntrinsic(rcvrTy, name) match {
            case List() => lookupNonintrinsicMethods(rcvrTy, name)
            case intrinsicSyms => intrinsicSyms
        }
    }
    
    // ___ Typed Paths ______________________________________________________   
    //
    // A Typed Path is simply a path where the symbols and type have been
    // determined.  A typed path may include error symbols if fields or local
    // variables found within are not defined.
    
    def typedPath(path: Path.Ref): Path.Typed = path match {
        case Path.Base(name) => {
            val sym = locals.get(name).getOrElse(Symbol.errorVar(name, None))
            Path.TypedBase(name, sym, sym.ty)
        }
        
        case Path.Field(base, name) => {
            val typedBase = typedPath(base)
            val sym = lookupFieldOrError(typedBase.ty, name, None)
            val subst = Subst(Path.This -> base)
            Path.TypedField(typedBase, sym, subst.ty(sym.ty))
        }
    }
    
    def typeOfPath(path: Path.Ref) = typedPath(path).ty
    
    // ___ Equating Paths ___________________________________________________   
    //
    // Two paths are equatable if they will always refer to the same object
    // at runtime.
    
    class Equater extends TransitiveCloser[Path.Ref] {
        protected[this] def successors(P1: Path.Ref): Iterable[Path.Ref] = {
            val byField = P1 match {
                case Path.Base(_) => Set()
                case Path.Field(base, name) => {
                    compute(base).map(Path.Field(_, name))
                }
            }
            val byRel = pathRels.flatMap {
                case (P1, PcEq, p2) => Some(p2)
                case (p2, PcEq, P1) => Some(p2)
                case _ => None
            }
            byField ++ byRel
        }
    }
    
    def equatable(path: Path.Ref) = new Equater().compute(path)
    def pathsAreEquatable(path1: Path.Ref, path2: Path.Ref) = equatable(path1) contains path2
    
    // ___ Bounding Type Variables __________________________________________
    
    class Bounder(Rel: TcRel) extends TransitiveCloser[Type.Ref] {
        protected[this] def successors(ty: Type.Ref) = ty match {
            case tyVar: Type.Var => typeVarBounds(tyVar)
            case _ => Nil
        }

        private[this] def typeVarBounds(tyVar: Type.Var) = {
            val TyVarName = tyVar.typeVar
            
            def boundsFromClassType(tyClass: Type.Class) = {
                // Add bounds from class:
                val classBounds = List[Type.Ref]() // XXX
                
                // Add bounds from type arguments in tyClass:
                tyClass.typeArgs.foldLeft(classBounds) { 
                    case (l, Type.TypeArg(TyVarName, TcEq, ty)) => ty :: l
                    case (l, Type.TypeArg(TyVarName, Rel, ty)) => ty :: l
                    case (l, _) => l
                }
            }
            
            typeOfPath(tyVar.path) match {
                case tyClass: Type.Class => boundsFromClassType(tyClass)
                case tyVar: Type.Var => {
                    compute(tyVar).toList.flatMap {
                        case tyClass: Type.Class => boundsFromClassType(tyClass)
                        case _ => Nil
                    }
                }
                
                case Type.Tuple(_) => Nil
                case Type.Null => Nil
            }
        }
    }
       
    def upperBoundType(ty: Type.Ref) = new Bounder(TcSub).compute(ty)
    def lowerBoundType(ty: Type.Ref) = new Bounder(TcSup).compute(ty)
    
    // ___ Argument Suitability _____________________________________________
    //
    // Argument suitability is used to resolve overloaded arguments.  We do not
    // consider the full subtyping relation but rather only the erased type
    // and (to a limited extent) type variables.
    
    private[this] def symbolsSubclass(csym_sub: Symbol.Class, csym_sup: Symbol.Class) = {
        val queued = new mutable.Queue[Symbol.Class]()
        val visited = new mutable.HashSet[Symbol.Class]()
        queued += csym_sub
        while(!visited(csym_sup) && !queued.isEmpty) {
            val csym_next = queued.dequeue()
            visited += csym_next
            queued ++= csym_next.superClassNames(state).map(state.symtab.classes).filterNot(visited)
        }
        visited(csym_sup)
    }
    
    private[this] def isSuitableArgumentBounded(ty_val: Type.Ref, ty_pat: Type.Ref): Boolean = {
        (ty_val, ty_pat) match {
            case (Type.Class(name_val, _), Type.Class(name_pat, _)) => {
                val sym_val = state.symtab.classes(name_val)
                val sym_pat = state.symtab.classes(name_pat)
                symbolsSubclass(sym_val, sym_pat)                
            }
            
            case (Type.Var(path_val, tvar_val), Type.Var(path_pat, tvar_pat)) if tvar_val == tvar_pat =>
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
        (upperBoundType(ty_val) cross lowerBoundType(ty_pat)).exists {
            case (u, l) => isSuitableArgumentBounded(u, l)
        }
    }
    
//    def isSubclass(ty_sub: Type.Ref, ty_sup: Type.Ref): Boolean = {
//        (ty_sub, ty_sup) match {
//            case (Type.Var(path_sub, var_sub), Type.Var(path_sup, var_sup)) => false // XXX
//            
//            case (Type.Class(name_sub, args_sub), Type.Class(name_sup, arg_sup)) => false // XXX
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
  
}