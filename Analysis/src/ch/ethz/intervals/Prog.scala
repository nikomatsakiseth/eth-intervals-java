package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.util.parsing.input.Positional
import scala.collection.immutable.ListSet
import scala.collection.immutable.Map

import Util._
import ch.ethz.intervals.log.Log
import ch.ethz.intervals.log.LogStack

class Prog(
    val logStack: LogStack,
    val cds_user: List[ir.ClassDecl],
    val cds_sys: List[ir.ClassDecl]
) {
    import logStack.log
    def errors = logStack.errors
    
    val env_empty = TcEnv(
        this,
        None,
        ir.t_void,
        Map(),
        FlowEnv.empty
    )        

    // ___ Class table ______________________________________________________
    
    val classDecls = cds_user ++ cds_sys    
    val classTable = Util.nameMap[ir.ClassName, ir.ClassDecl](classDecls)
    def classDecl(c: ir.ClassName) = classTable.get(c) match {
        case Some(cd) => cd
        case None => throw new CheckFailure("intervals.no.such.class", c)
    }
    
    // ___ Computed results _________________________________________________
    //
    // When we check a class, we store the exported environments from each 
    // of its constructors in this table.  These can be used by subclasses.
    
    var exportedCtorEnvs = Map(
        ((ir.c_object, ir.m_init), FlowEnv.empty),
        ((ir.c_interval, ir.m_init), FlowEnv.empty)
    )
    
    // ___ Fresh variables __________________________________________________
    
    private var counter = 0
    def fresh(nm: String) = {
        val c = counter
        counter = counter + 1
        "%s[%d]".format(nm, c)
    }    
    
    def freshVarName = ir.VarName(fresh("tmp"))
    
    // ___ Basic type operations ____________________________________________
    
    // Is c_sub an erased subtype of class c_sup?
    def isSubclass(c_sub: ir.ClassName, c_sup: ir.ClassName): Boolean = {
        (c_sub == c_sup) || {            
            val cd_sub = classDecl(c_sub)
            cd_sub.superClasses.exists { c => isSubclass(c, c_sup) }
        }
    }
    
    // Is wt an erased subtype of class c?
    def isSubclass(wt: ir.WcTypeRef, c: ir.ClassName): Boolean =
        isSubclass(wt.c, c)
        
    def strictSuperclasses(c0: ir.ClassName): Set[ir.ClassName] = {
        def accumulate(sc: Set[ir.ClassName], c: ir.ClassName): Set[ir.ClassName] = {
            classDecl(c).superClasses.foldLeft(sc + c)(accumulate)
        }
        accumulate(Set.empty, c0) - c0
    }
    
    /// Higher-order function that takes a func 'func' which adds
    /// values of type X for a given class.  'func' is applied
    /// to 'c' and all its supertypes in a bottom-up fashion starting
    /// from Object.
    def addClassAndSuperclasses[X](
        func: ((X, ir.ClassName) => X)
    )(
        set0: X, c: ir.ClassName
    ): X = {
        val cd = classDecl(c)
        val set1 = cd.superClasses.foldLeft(set0)(addClassAndSuperclasses(func))
        func(set1, c)
    }
    
    /// Ghost fields declared:
    def addGhostFieldsDeclaredOnClass(gfds0: Set[ir.GhostFieldDecl], c: ir.ClassName) =
        classDecl(c).fields.foldLeft(gfds0) {
            case (gfds, gfd: ir.GhostFieldDecl) => gfds + gfd
            case (gfds, _) => gfds
        }
    def ghostFieldsDeclaredOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addGhostFieldsDeclaredOnClass)(ListSet.empty, c)
    
    /// Ghosts (bound ghost fields) on a class:
    def addGhostsOnClass(fs0: Set[ir.Ghost], c: ir.ClassName) =
        fs0 ++ classDecl(c).ghosts
    def ghostsOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addGhostsOnClass)(ListSet.empty, c)        
        
    /// Type variables declared:
    def addTypeVarsDeclaredOnClass(s0: Set[ir.TypeVar], c: ir.ClassName) =
        s0 ++ classDecl(c).typears        
    def typeVarsDeclaredOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addClassAndSuperclasses)(ListSet.empty, c)
        
    /// Type arguments (bound type variables) on a class:
    def addTypeArgsOnClass(s0: Set[ir.TypeArg], c: ir.ClassName) =
        s0 ++ classDecl(c).typeArgs
    def typeArgsOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addTypeArgsOnClass)(ListSet.empty, c)
        
    /// All ghost fields declared on c or a superclass which have not been bound.
    def unboundGhostFieldsOnClassAndSuperclasses(c: ir.ClassName) = {
        val gfds = ghostFieldsDeclaredOnClassAndSuperclasses(c)
        val boundFields = ghostsOnClassAndSuperclasses(c).map(_.f)
        gfds.filter(gfd => !boundFields(gfd.name))
    }
    
    /// All type variables declared on c or a superclass which have not been bound.
    def unboundTypeVarsOnClassAndSuperclasses(c: ir.ClassName) = {
        val tvds = typeVarsDeclaredOnClassAndSuperclasses(c)
        val boundTypeVars = typeArgsOnClassAndSuperclasses(c).map(_.tv)
        tvds.filter(tvd => !boundTypeVars(tvd.name))
    }
    
    /// Augments ghostSubstOfType with a mapping 'this.ctor→this.super'
    def superSubstOfClass(c: ir.ClassName) = {
        val cd = classDecl(c)
        PathSubst.pp(ir.p_ctor, ir.p_super)
    }

    /// For a class c with unbound ghost fields F, yields a type c<F: this.F>{as}
    def thisTref(cd: ir.ClassDecl, as: ir.Attrs): ir.TypeRef = {
        val gfds = unboundGhostFieldsOnClassAndSuperclasses(cd.name).toList
        ir.TypeRef(cd.name, gfds.map(_.ghost), as)
    }    

    /// For a class c with unbound ghost fields F, yields a type c<F: this.F>
    def thisTref(cd: ir.ClassDecl): ir.TypeRef =
        thisTref(cd, ir.noAttrs)
        
    // ___ Type Hierarchy ___________________________________________________
    
    /// Supertypes:
    def supertypesOfClass(c: ir.ClassName) = log.indented("supertypesOfClass(%s)", c) {
        val cd = classDecl(c)
        cd.superClasses.map { c => 
            ir.ClassType(c, ir.Ghost(ir.f_ctor, ), cd.ghosts, cd.typeArgs, ir.noAttrs)
        }
    }
    def sups(ct0: ir.ClassType) = log.indented("sups(%s)", ct0) {
        supertypesOfClass(ct0.c).map { ct =>
            ir.ClassType(ct.c, t.ghosts ++ cd.ghosts, t.typeArgs ++ cd.typeArgs, ct.as)
        }
    }
    
    ///
    def searchClassAndSuperclasses[X](
        func: ((ir.ClassDecl) => Option[X])
    )(
        ct: ir.ClassType, 
    ): Option[(ir.ClassType, X)] = {
        val cd = classDecl(ct)
        func(cd) match {
            case Some(x) =>
                Some((ct, x))
                
            case None =>
                sups(ct).firstSomeReturned(searchClassAndSuperclasses(func))
        }
    }
    
    /// Field decl for t0::f 
    def fieldDecl(ct0: ir.ClassType, f: ir.FieldName): ir.FieldDecl = {
        log.indented("fieldDecl(%s,%s)", ct0, f) {
            def extractField(ct1: ir.ClassType, cd: ir.ClassDecl) =
                cd.fields.find(_.name == f)
                
            searchClassAndSuperclasses(extractField)(ct) match {
                case Some((_, fd)) => fd
                case None => throw new CheckFailure("intervals.no.such.field", c0, f)
            }
        }            
    }

    /// Method sig for c0::m()
    def methodSig(ct0: ir.ClassType, m: ir.MethodName): Option[ir.MethodSig] = {
        log.indented("methodSig(%s,%s)", ct0, m) {
            def extractMethod(cd: ir.ClassDecl) =
                cd.methods.find(_.name == f).map(_.msig(thisTref(cd)))
                
            searchClassAndSuperclasses(extractMethod)(ct).map(_._2)
        }
    }
    
    /// Returns the (potentially super-)type that defined the method 'm' in the class 'c'
    def typeOriginallyDefiningMethod(c: ir.ClassName, m: ir.MethodName): Option[ir.TypeRef] = {
        val cd = classDecl(c)
        val subst = superSubstOfClass(c)
        cd.superClasses.firstSomeReturned(c_sup => 
            typeOriginallyDefiningMethod(c_sup, m).map(subst.tref)
        ).orElse(
            if(cd.methods.exists(_.name == m)) Some(thisTref(cd))
            else None
        )
    }    
    
    /// Returns the signatures for any methods 'm' defined in the supertypes of 'c'.
    def overriddenMethodSigs(c: ir.ClassName, m: ir.MethodName): List[ir.MethodSig] = {
        val cd = classDecl(c)
        val subst = superSubstOfClass(c)
        cd.superClasses.foldRight(List[ir.MethodSig]()) { case (c_sup, l) =>
            methodSig(c_sup, m).map(subst.methodSig) match {
                case None => l
                case Some(msig) => msig :: l
            }
        }
    }
    
    
}