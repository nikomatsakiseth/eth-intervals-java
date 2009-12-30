package ch.ethz.intervals

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Positional
import scala.collection.immutable.ListSet

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
    // Fresh Variables
    
    private var counter = 0
    def fresh(nm: String) = {
        val c = counter
        counter = counter + 1
        "%s[%d]".format(nm, c)
    }    
    
    // ______________________________________________________________________
    // Errors
    
    val errors = new ListBuffer[ir.Error]
    def report(loc: Positional, msg: String, args: Any*) {
        val argList = args.toList.map(_.toString)
        errors += ir.Error(loc, msg, argList)
    }

    def reportError(loc: Positional, i: ir.IrError) {
        report(loc, i.msg, i.args: _*)
    }

    def at[R](loc: Positional, default: R)(g: => R): R = 
        log.indentedRes(loc) {
            try {
                g
            } catch {
                case err: ir.IrError =>
                    reportError(loc, err)
                    log("Error: %s", err)
                    default           
            }            
        }
        
    // ______________________________________________________________________
    // Basic Type Operations: Finding fields, methods, supertypes
    
    // Is wt an erased subtype of class c?
    def isSubclass(wt: ir.WcTypeRef, c: ir.ClassName): Boolean = {
        (wt.c == c) || {            
            val cd = classDecl(wt.c)
            cd.superTypes.exists { t => isSubclass(t, c) }
        }
    }
    
    def strictSuperclasses(c0: ir.ClassName): Set[ir.ClassName] = {
        def accumulate(sc: Set[ir.ClassName], c: ir.ClassName): Set[ir.ClassName] = {
            classDecl(c).superTypes.map(_.c).foldLeft(sc + c)(accumulate)
        }
        accumulate(Set.empty, c0) - c0
    }
    
    // Collection of all ghost fields declared on type 'c_0'.
    // A ghost field from a supertype c_1 is included so long as there is path
    // from c_0 to c_1 that does not define the value of the ghost.
    def ghostFieldDecls(c_0: ir.ClassName): List[ir.GhostFieldDecl] = {
        def lgfd(c: ir.ClassName, suppress: Set[ir.FieldName]): List[ir.GhostFieldDecl] = {
            log.indentedRes("lgfd(%s)", c) {
                val cd = classDecl(c)
                val lgfd_super = cd.superTypes.flatMap { t =>
                    lgfd(t.c, suppress ++ t.ghosts.map(_.f))
                }            
                cd.fields.foldLeft(lgfd_super) {
                    case (l, gfd: ir.GhostFieldDecl) if !suppress(gfd.name) => gfd :: l
                    case (l, _) => l
                }                
            }
        }
        lgfd(c_0, ListSet.empty)
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
    def superSubstOfType(t: ir.TypeRef) =
        PathSubst.pp(ir.p_ctor, ir.p_super) + ghostSubstOfType(t)

    /// For a class c with ghost fields F, yields a type c<F: this.F>{as}
    def thisTref(cd: ir.ClassDecl, as: ir.Attrs): ir.TypeRef = {
        val lgfds = ghostFieldDecls(cd.name)
        ir.TypeRef(cd.name, lgfds.map(_.ghost), as)
    }    

    /// For a class c with ghost fields F, yields a type c<F: this.F>
    def thisTref(cd: ir.ClassDecl): ir.TypeRef =
        thisTref(cd, ir.noAttrs)
    
    /// supertypes of t
    def sups(t: ir.TypeRef): List[ir.TypeRef] = {
        val cd = classDecl(t.c)
        lazy val subst_t = ghostSubstOfType(t)        
        cd.superTypes.map { case t_1 =>
            val t_extends = ghostSubstOfType(t_1).tref(thisTref(classDecl(t_1.c)))
            subst_t.tref(t_extends)
        }
    }
    
    /// Field decl for t0::f 
    def fieldDecl(c0: ir.ClassName, f: ir.FieldName): ir.FieldDecl = {
        log.indentedRes("fieldDecl(%s,%s)", c0, f) {
            def search(c: ir.ClassName): Option[ir.FieldDecl] = 
                log.indentedRes("search(%s)", c) {
                    val cd = classDecl(c)
                    cd.fields.find(_.name == f) match {
                        case Some(fd) => Some(fd)
                        case None => cd.superTypes.firstSomeReturned { t =>
                            search(t.c).map(superSubstOfType(t).fieldDecl)
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
                    case None => cd.superTypes.firstSomeReturned { t =>
                        search(t.c).map(superSubstOfType(t).methodSig)
                    }
                }
            }
            search(c0)
        }
    }
    
    /// Returns the (potentially super-)type that defined the method 'm' in the class 'c'
    def typeOriginallyDefiningMethod(c: ir.ClassName, m: ir.MethodName): Option[ir.TypeRef] = {
        val cd = classDecl(c)
        cd.superTypes.firstSomeReturned(t_sup => 
            typeOriginallyDefiningMethod(t_sup.c, m).map(superSubstOfType(t_sup).tref)
        ).orElse(
            if(cd.methods.exists(_.name == m)) Some(thisTref(cd))
            else None
        )
    }    
    
    /// Returns the signatures for any methods 'm' defined in the supertypes of 'c'.
    def overriddenMethodSigs(c: ir.ClassName, m: ir.MethodName): List[ir.MethodSig] = {
        val cd = classDecl(c)
        cd.superTypes.foldRight(List[ir.MethodSig]()) { case (t, l) =>
            methodSig(t.c, m).map(superSubstOfType(t).methodSig) match {
                case None => l
                case Some(msig) => msig :: l
            }
        }
    }
    
    
}