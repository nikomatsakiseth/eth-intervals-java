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
        prog        = this,
        c_this      = ir.c_void,
        o_lv_cur    = None,
        wt_ret      = ir.t_void,
        identityRet = List(),
        perm        = Map(),
        flow        = FlowEnv.empty
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

    private val fresh = new Fresh("prog")
    def freshVarName = ir.VarName(fresh.next())
    
    // ___ Basic type operations ____________________________________________
    
    /** Is `c_sub` an erased subtype of class `c_sup`? */
    def isStrictSubclass(c_sub: ir.ClassName, c_sup: ir.ClassName): Boolean = {
        val cd_sub = classDecl(c_sub)
        cd_sub.superClasses.exists { c => isSubclass(c, c_sup) }
    }
    
    /** Is `c_sub` an erased subtype of class `c_sup`? */
    def isSubclass(c_sub: ir.ClassName, c_sup: ir.ClassName): Boolean = {
        (c_sub == c_sup) || isStrictSubclass(c_sub, c_sup)
    }
    
    /** Is `c` an interface class? */
    def isInterface(c: ir.ClassName) = classDecl(c).attrs.interface
    
    /** Is `c` an interface class? */
    def isNotInterface(c: ir.ClassName) = !isInterface(c)
    
    def classAndSuperclasses(c0: ir.ClassName): Set[ir.ClassName] = {
        def accumulate(sc: Set[ir.ClassName], c: ir.ClassName): Set[ir.ClassName] = {
            classDecl(c).superClasses.foldLeft(sc + c)(accumulate)
        }
        accumulate(Set.empty, c0)
    }
    
    def strictSuperclasses(c0: ir.ClassName) = classAndSuperclasses(c0) - c0
    
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
        gfds0 ++ classDecl(c).ghostFieldDecls
    def ghostFieldsDeclaredOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addGhostFieldsDeclaredOnClass)(ListSet.empty, c)
    
    /// Ghosts (bound ghost fields) on a class:
    def addGhostsOnClass(fs0: Set[ir.Ghost], c: ir.ClassName) =
        fs0 ++ classDecl(c).ghosts
    def ghostsOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addGhostsOnClass)(ListSet.empty, c)        
        
    /// Type variables declared:
    def addTypeVarsDeclaredOnClass(s0: Set[ir.TypeVarDecl], c: ir.ClassName) =
        s0 ++ classDecl(c).typeVarDecls
    def typeVarsDeclaredOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addTypeVarsDeclaredOnClass)(ListSet.empty, c)
        
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
    def unboundTypeVarsDeclaredOnClassAndSuperclasses(c: ir.ClassName) = {
        val tvds = typeVarsDeclaredOnClassAndSuperclasses(c)
        val boundTypeVars = typeArgsOnClassAndSuperclasses(c).map(_.tv)
        tvds.filter(tvd => !boundTypeVars(tvd.name))
    }
        
}