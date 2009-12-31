package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.util.parsing.input.Positional
import scala.collection.immutable.ListSet
import scala.collection.immutable.Map

import Util._

class Prog(
    val log: Log,
    val cds_user: List[ir.ClassDecl]
) {
    val classDecls = cds_user ++ ir.cds_default
    
    // ______________________________________________________________________
    // Class Table
    
    val classTable = Util.nameMap[ir.ClassName, ir.ClassDecl](classDecls)
    def classDecl(c: ir.ClassName) = classTable.get(c) match {
        case Some(cd) => cd
        case None => throw ir.IrError("intervals.no.such.class", c)
    }
    
    // ______________________________________________________________________
    // Computed Results
    //
    // When we check a class, we store the exported environments from each 
    // of its constructors in this table.  These can be used by subclasses.
    
    var exportedCtorEnvs = Map.empty[(ir.ClassName, ir.MethodName), ir.TcEnv]
    
    // ______________________________________________________________________
    // Fresh Variables
    
    private var counter = 0
    def fresh(nm: String) = {
        val c = counter
        counter = counter + 1
        "%s[%d]".format(nm, c)
    }    
    
    def freshVarName = ir.VarName(fresh("tmp"))
    
    // ______________________________________________________________________
    // Errors
    
    var errors = ListSet.empty[ir.Error] // use a list set to keep ordering
    
    def report(loc: Positional, msg: String, args: Any*) {
        val argList = args.toList.map(_.toString)
        log("Error: %s(%s)", msg, argList.mkString(", "))
        errors += ir.Error(loc.pos, msg, argList)
    }

    def reportError(loc: Positional, i: ir.IrError) {
        report(loc, i.msg, i.args: _*)
    }

    def at[R](loc: Positional, default: R)(g: => R): R = 
        log.indentedRes(loc) {
            try { g } catch {
                case err: ir.IrError =>
                    reportError(loc, err)
                    default           
            }            
        }
        
    // ______________________________________________________________________
    // Basic Type Operations: Finding fields, methods, supertypes
    
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
    
    // Collection of all ghost fields declared on type 'c_0'.
    // A ghost field from a supertype c_1 is included so long as there is path
    // from c_0 to c_1 that does not define the value of the ghost.
    def ghostFieldDecls(c_0: ir.ClassName): List[ir.GhostFieldDecl] =
        log.indentedRes("ghostFieldDecls(%s)", c_0) {
            val cd = classDecl(c_0)
            log("ghosts: %s", cd.ghosts)
            
            // Ghosts declared on cd:
            val gfds_cd = cd.fields.foldLeft[Set[ir.GhostFieldDecl]](ListSet.empty) { 
                case (s, gfd: ir.GhostFieldDecl) => s + gfd
                case (s, _) => s
            }
            
            // + Ghosts declared on its supertypes:
            val gfds_super = cd.superClasses.foldLeft(gfds_cd) { case (s, c) => 
                s ++ ghostFieldDecls(c)
            }
            
            // - Ghosts defined in cd:
            val fs_defd = cd.ghosts.map(_.f)            
            gfds_super.filter(gfd => !fs_defd.contains(gfd.name)).toList
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

    /// For a class c with ghost fields F, yields a type c<F: this.F>{as}
    def thisTref(cd: ir.ClassDecl, as: ir.Attrs): ir.TypeRef = {
        val gfds = ghostFieldDecls(cd.name)
        ir.TypeRef(cd.name, gfds.map(_.ghost), as)
    }    

    /// For a class c with ghost fields F, yields a type c<F: this.F>
    def thisTref(cd: ir.ClassDecl): ir.TypeRef =
        thisTref(cd, ir.noAttrs)
        
    def supertypesOfClass(c: ir.ClassName) = {
        val cd = classDecl(c)
        val subst = superSubstOfClass(c)
        cd.superClasses.map { c => subst.tref(thisTref(cd)) }
    }

    /// supertypes of t
    def sups(t: ir.TypeRef): List[ir.TypeRef] = {
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
                case None => throw ir.IrError("intervals.no.such.field", c0, f)
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