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
        ((ir.c_object, ir.m_init), ir.Env.empty),
        ((ir.c_interval, ir.m_init), ir.Env.empty)
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
    
    /// Adds the ghost fields declared on class 'c' (not its supertypes).
    def addGhostFieldsDeclaredOnClass(gfds0: Set[ir.GhostFieldDecl], c: ir.ClassName) =
        classDecl(c).fields.foldLeft(gfds0) {
            case (gfds, gfd: ir.GhostFieldDecl) => gfds + gfd
            case (gfds, _) => gfds
        }

    /// All ghost field declared on c or one of its superclasses.
    def ghostFieldsDeclaredOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addGhostFieldsDeclaredOnClass)(ListSet.empty, c)
    
    /// Adds the ghost fields bound on 'c' (not its supertypes).
    def addGhostFieldsBoundOnClass(fs0: Set[ir.FieldName], c: ir.ClassName) =
        classDecl(c).ghosts.foldLeft(fs0) { case (fs, g) => fs + g.f }
        
    /// All ghost field bound on c or one of its superclasses.
    def ghostFieldsBoundOnClassAndSuperclasses(c: ir.ClassName) =
        addClassAndSuperclasses(addGhostFieldsBoundOnClass)(ListSet.empty, c)
        
    /// All ghost fields declared on c or a superclass which have not been bound.
    def unboundGhostFieldsOnClassAndSuperclasses(c: ir.ClassName) = {
        val gfds = ghostFieldsDeclaredOnClassAndSuperclasses(c)
        val boundFields = ghostFieldsBoundOnClassAndSuperclasses(c)
        gfds.filter(gfd => !boundFields(gfd.name))
    }
    
    /// For a type 't=c<F: P>' creates a substitution '[this.F→P]'.
    /// Ghosts F'#F defined on the class c are not substituted.
    /// @see ghostSubstOfTeePee()
    def ghostSubstOfType(t: ir.TypeRef): PathSubst = 
        PathSubst.pp(
            t.ghosts.map(g => ir.p_this + g.f),
            t.ghosts.map(g => g.p)
        )        
    
    /// Augments ghostSubstOfType with a mapping 'this.ctor→this.super'
    def superSubstOfClass(c: ir.ClassName) = {
        val cd = classDecl(c)
        PathSubst.pp(
            ir.p_ctor  :: cd.ghosts.map(_.f.thisPath),
            ir.p_super :: cd.ghosts.map(_.p)
        )        
    }

    /// For a class c with unbound ghost fields F, yields a type c<F: this.F>{as}
    def thisTref(cd: ir.ClassDecl, as: ir.Attrs): ir.TypeRef = {
        val gfds = unboundGhostFieldsOnClassAndSuperclasses(cd.name).toList
        ir.TypeRef(cd.name, gfds.map(_.ghost), as)
    }    

    /// For a class c with unbound ghost fields F, yields a type c<F: this.F>
    def thisTref(cd: ir.ClassDecl): ir.TypeRef =
        thisTref(cd, ir.noAttrs)
        
    def supertypesOfClass(c: ir.ClassName) = log.indentedRes("supertypesOfClass(%s)", c) {
        val cd = classDecl(c)
        val subst = superSubstOfClass(c)
        cd.superClasses.map { c => subst.tref(thisTref(classDecl(c))) }
    }

    /// supertypes of t
    def sups(t: ir.TypeRef): List[ir.TypeRef] = log.indentedRes("sups(%s)", t) {
        val subst = ghostSubstOfType(t)
        supertypesOfClass(t.c).map(subst.tref)
    }
    
    /// Field decl for t0::f 
    def fieldDecl(c0: ir.ClassName, f: ir.FieldName): ir.FieldDecl = {
        log.indentedRes("fieldDecl(%s,%s)", c0, f) {
            def search(c: ir.ClassName): Option[ir.FieldDecl] = 
                log.indentedRes("search(%s)", c) {
                    val cd = classDecl(c)
                    cd.fields.find(_.name == f) match {
                        case Some(fd) => Some(fd)
                        case None => 
                            val subst = superSubstOfClass(c)
                            cd.superClasses.firstSomeReturned { c_sup =>
                                search(c_sup).map(subst.fieldDecl)
                            }
                    }
                }
            search(c0) match {
                case None => throw new CheckFailure("intervals.no.such.field", c0, f)
                case Some(fd) => fd
            }
        }            
    }

    /// Method sig for c0::m()
    def methodSig(c0: ir.ClassName, m: ir.MethodName): Option[ir.MethodSig] = {
        log.indentedRes("methodSig(%s,%s)", c0, m) {
            def search(c: ir.ClassName): Option[ir.MethodSig] = {
                val cd = classDecl(c)
                cd.methods.find(_.name == m) match {
                    case Some(md) => 
                        Some(md.msig(thisTref(cd)))
                    case None => 
                        val subst = superSubstOfClass(c)
                        cd.superClasses.firstSomeReturned { c_sup =>
                            search(c_sup).map(subst.methodSig)
                        }
                }
            }
            search(c0)
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