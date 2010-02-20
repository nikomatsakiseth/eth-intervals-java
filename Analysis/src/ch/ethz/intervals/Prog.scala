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
        ir.c_void,
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
    //
    // 
    
    private var dictWords = {
        try {
            val dictFile = new java.io.File("/usr/share/dict/words")
            val dictSource = scala.io.Source.fromFile(dictFile)
            val lines = scala.util.Random.shuffle(dictSource.getLines("\n").toList)
            dictSource.close()
            lines            
        } catch {
            case _: java.io.IOException => List()
        }
    }
    
    private var counter = 0
    def fresh(nm: String) = {
        dictWords match {
            case word :: tl =>
                dictWords = tl
                "[%s]".format(word)
            case List() =>
                val c = counter
                counter = counter + 1
                "%s[%d]".format(nm, c)
        }        
    }    
    
    def freshVarName = ir.VarName(fresh("tmp"))
    
    // ___ Basic type operations ____________________________________________
    
    // Is c_sub an erased subtype of class c_sup?
    def isStrictSubclass(c_sub: ir.ClassName, c_sup: ir.ClassName): Boolean = {
        val cd_sub = classDecl(c_sub)
        cd_sub.superClasses.exists { c => isSubclass(c, c_sup) }
    }
    
    // Is c_sub an erased subtype of class c_sup?
    def isSubclass(c_sub: ir.ClassName, c_sup: ir.ClassName): Boolean = {
        (c_sub == c_sup) || isStrictSubclass(c_sub, c_sup)
    }
    
    def classAndSuperclasses(c0: ir.ClassName): Set[ir.ClassName] = {
        def accumulate(sc: Set[ir.ClassName], c: ir.ClassName): Set[ir.ClassName] = {
            classDecl(c).superClasses.foldLeft(sc + c)(accumulate)
        }
        accumulate(Set.empty, c0) - c0
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
    
    // ___ Type Hierarchy ___________________________________________________
    
    /// Supertypes:
    def supertypesOfClass(c: ir.ClassName) = log.indented("supertypesOfClass(%s)", c) {
        val cd = classDecl(c)
        cd.superClasses.map { c => 
            ir.ClassType(c, cd.ghosts, cd.typeArgs)
        }
    }
    def sups(wct: ir.WcClassType) = log.indented("sups(%s)", wct) {
        supertypesOfClass(wct.c).map { ct_sup =>
            ir.WcClassType(
                ct_sup.c,
                wct.wghosts ++ ct_sup.ghosts, 
                wct.wtargs ++ ct_sup.targs
            )
        }
    }
    
    ///
    def searchClassAndSuperclasses[X](
        func: (ir.ClassDecl => Option[X])
    )(
        wct: ir.WcClassType
    ): Option[(ir.WcClassType, X)] = {
        val cd = classDecl(wct.c)
        func(cd) match {
            case Some(x) =>
                Some((wct, x))
                
            case None =>
                sups(wct).firstSomeReturned(searchClassAndSuperclasses(func) _)
        }
    }
    
    /// Field decl for t0::f 
    def reifiedFieldDecl(wct: ir.WcClassType, f: ir.FieldName): Option[(ir.WcClassType, ir.ReifiedFieldDecl)] = {
        log.indented("fieldDecl(%s,%s)", wct, f) {
            def extractField(cd: ir.ClassDecl) = cd.reifiedFieldDecls.find(_.isNamed(f))
            searchClassAndSuperclasses(extractField)(wct)
        }            
    }

    /// Method decl for c0::m()
    def methodDecl(wct: ir.WcClassType, m: ir.MethodName): Option[(ir.WcClassType, ir.MethodDecl)] = {
        log.indented("methodSig(%s,%s)", wct, m) {
            def extractMethod(cd: ir.ClassDecl) = cd.methods.find(_.isNamed(m))
            searchClassAndSuperclasses(extractMethod)(wct)
        }
    }
    
    /// Returns the signatures for any methods 'm' defined in the supertypes of 'c'.
    def overriddenMethodSigs(wct: ir.WcClassType, m: ir.MethodName): List[ir.MethodSig] = {
        val cd = classDecl(wct.c)
        sups(wct).foldRight(List[ir.MethodSig]()) { case (wct_sup, l) =>
            methodDecl(wct_sup, m) match {
                case None => l
                case Some((wct_rcvr, md)) => md.msig(wct_rcvr) :: l
            }
        }
    }
    
    
}