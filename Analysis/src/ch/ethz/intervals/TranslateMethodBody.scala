package ch.ethz.intervals

import com.sun.source.tree._
import com.sun.source.tree.Tree.{Kind => TRK}
import checkers.types.AnnotatedTypeMirror
import checkers.types.AnnotatedTypeMirror._
import checkers.types.AnnotatedTypeFactory
import checkers.source.Result
import checkers.util.{ElementUtils => EU}
import checkers.util.{TreeUtils => TU}
import checkers.util.{AnnotationUtils => AU}
import checkers.util.{InternalUtils => IU}
import javax.annotation.processing.ProcessingEnvironment
import javax.lang.model.element._
import javax.lang.model.element.Name
import javax.lang.model.`type`._
import javax.lang.model.element.{ElementKind => EK}
import javax.lang.model.`type`.{TypeKind => TK}
import javax.lang.model.util.{ElementFilter => EF}
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Map
import scala.collection.immutable.ListMap
import scala.collection.immutable.Set
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import java.util.{List => jList}
import Util._
import ch.ethz.intervals.quals._
import ch.ethz.intervals.log.LogStack

object TranslateMethodBody
{
    def apply(
        logStack: LogStack, 
        ttf: TranslateTypeFactory, 
        mtree: MethodTree
    ): ir.StmtSeq = logStack.log.indented("translateMethodBody(%s)", mtree) {
        import logStack.log
        import ttf.TreePosition
        import ttf.fieldName
        import ttf.methodName
        import ttf.wke

        // ___ Miscellany _______________________________________________________

        class Unhandled(val tree: Tree) 
        extends RuntimeException("Could not handle: "+tree.getKind+" "+tree)

        val fresh = new Fresh("jv")
        def freshVar() = ir.VarName(fresh.next())
        def nm(elem: Element) = elem.getSimpleName.toString

        // ___ Non-wildcard checking ____________________________________________
        //
        // When creating new objects, wildcards are not permitted.  Rather than have
        // two parsing routines etc, we just check that the user doesn't use any.

        def chkNoWcInPath(wp: ir.WcPath) = wp match {
            case p: ir.Path => p
            case _ => throw new CheckFailure("intervals.wildcard.not.perm", wp)
        }

        def chkNoWcInTarg(wta: ir.WcTypeArg) = wta match {
            case ta: ir.TypeArg => ta
            case _ => throw new CheckFailure("intervals.wildcard.not.perm", wta)
        }

        def chkNoWcInGhost(wg: ir.WcGhost) = 
            ir.Ghost(wg.f, chkNoWcInPath(wg.wp))
            
        def chkNoWcInTref(wt: ir.WcTypeRef): ir.ClassType =
            wt match {
                case wct: ir.WcClassType =>
                    ir.ClassType(
                        wct.c, 
                        wct.wghosts.map(chkNoWcInGhost), 
                        wct.wtargs.map(chkNoWcInTarg))
                case _ => throw new CheckFailure("intervals.can.only.create.classes", wt)
            }

        // ___ Symbols and Versions _____________________________________________

        sealed class Symbol(
            val name: String, 
            val isFinal: Boolean,
            val annty: AnnotatedTypeMirror, 
            val wtref: ir.WcTypeRef
        ) {
            var maxVersion = 0

            // Version of the symbol used for method parameters:
            //   This way, the names of method parameters match up
            //   with those we find when extracting only the interface.
            def initialParameterVersion = InitialParameterVersion(this)
            
            // Version of the symbol resulting from a parameter decl or phi node.
            def nextDefinedVersion = {
                maxVersion += 1
                LocalVersion(this, maxVersion)                    
            }

            // Version of the symbol resulting from an assignment.
            def nextExprVersion(lv: ir.VarName) = {
                ExprVersion(this, lv)
            }

            override def toString = "Symbol(%s)".format(name)
        }

        abstract class Version {
            val sym: Symbol
            val lv: ir.VarName
        }    
        sealed case class InitialParameterVersion(sym: Symbol) extends Version {
            val lv = ir.VarName(sym.name)
        }
        sealed case class LocalVersion(sym: Symbol, ver: Int) extends Version {
            val lv = ir.VarName(sym.name + "[" + ver + "]")
            def toLvDecl = ir.LvDecl(lv, sym.wtref, List())
        }
        sealed case class ExprVersion(sym: Symbol, lv: ir.VarName) extends Version
        
        def replacePairs(pairs: List[(String,String)])(s0: String) = {
            pairs.foldLeft(s0) { case (s, pair) =>
                s.replace(pair._1, pair._2)
            }
        }

        class SymbolTable(m: Map[String, Version]) {
            def +(pair: (String, Version)) = new SymbolTable(m + pair)
            def ++(pairs: List[(String, Version)]) = new SymbolTable(m ++ pairs)
            def apply(s: String) = m(s)
            def get(s: String) = m.get(s)
            def toList = m.toList
            def foldLeft[B](z: B)(op: ((B, (String, Version)) => B)) = m.foldLeft(z)(op)

            lazy val replaceFunc: (String => String) = {
                val pairs = m.toList.map { case (nm, ver) => (ver.lv.toString, nm) }
                replacePairs(pairs)
            }
        }
        
        def SymbolTable(names: List[String], vers: List[Version]): SymbolTable =
            new SymbolTable(Map(names.zip(vers): _*))

        def createSymbol(env: ttf.TranslateEnv)(vtree: VariableTree): Symbol = {
            val elem = TU.elementFromDeclaration(vtree)
            val annty = ttf.getAnnotatedType(elem)
            val wtref = ttf.wtref(env, ir.wgs_constructed)(annty)
            new Symbol(nm(elem), EU.isFinal(elem), annty, wtref)
        }

        // ___ Error placement and printing _____________________________________
        
        def at[R](treePos: Tree, symtab: SymbolTable, tag: String, defFunc: => R)(func: => R) = {
            logStack.at(
                new DummyPositional(TreePosition(treePos, symtab.replaceFunc), tag), 
                defFunc
            )(func)            
        }

        // ___ Method environment, parameters ___________________________________
        
        val elem_mthd = TU.elementFromDeclaration(mtree)
        val elem_cls = EU.enclosingClass(elem_mthd)
        val env_mthd = ttf.elemEnv(elem_mthd)
        val elems_param = mtree.getParameters.map(TU.elementFromDeclaration).toList
        val syms_mthdParam = mtree.getParameters.map(createSymbol(env_mthd)).toList
        val vers_mthdParam = syms_mthdParam.map(_.initialParameterVersion)
        val symtab_mthdParam = SymbolTable(elems_param.map(nm), vers_mthdParam)

        // ___ Scopes ___________________________________________________________
        
        abstract class ScopeKind
        case object SkAnonBlock extends ScopeKind        // /* ... */
        case object SkSwitch extends ScopeKind           // switch(...) { ... }
        case object SkIf extends ScopeKind               // if(...) { ... }, (... ? ... : ...)
        case class SkInlineInterval(x: ir.VarName) extends ScopeKind
        case object SkTryCatch extends ScopeKind
        
        // Loops are actually encoded using three nested scopes.  See
        // the loopStmt() function for more details.
        case object SkLoopBreak extends ScopeKind
        case object SkLoopProper extends ScopeKind
        case object SkLoopContinue extends ScopeKind

        // The different kinds of targets indicate which kinds of scopes can be
        // targeted by a break vs a continue.  Note that unlike in Java we break each
        // loop in a Block scope, out of which the user can break, and a Loop scope,
        // to which the user can continue.
        abstract class BranchKind(
            val namedTargets: Set[ScopeKind],
            val anonTargets: Set[ScopeKind])
        case object BranchBreak extends BranchKind(
            Set(SkLoopBreak, SkSwitch, SkAnonBlock),
            Set(SkLoopBreak, SkSwitch))
        case object BranchContinue extends BranchKind(
            Set(SkLoopContinue),
            Set(SkLoopContinue))
        
        class Scope(
            val kind: ScopeKind,
            val parents: List[Scope],
            val o_label: Option[Name],
            val symtab_in: SymbolTable,
            val defines_xtra: List[ir.LvDecl]
        ) {
            override def toString = "Scope(%s,%s)".format(kind, o_label)
            
            // ___ Subseqs __________________________________________________________
            
            private val seqsBuffer = new ListBuffer[ir.StmtSeq]()
            def addSeq(seq: ir.StmtSeq) = seqsBuffer += seq
            
            // ___ Symbol Table and Flow of Control _________________________________
            //            
            // Defines are the variables defined on exit from this scope.  
            //
            // Auto defines are user-declared variables, whose most 
            // recent version is automatically carried from scope-to-scope.  
            //
            // Xtra defines are special purpose, anonymous defines 
            // such as the result of a (c ? t : f) expression.
            
            def make_auto_defines = {
                symtab_in.toList.flatMap { 
                    case (_, ver) if ver.sym.isFinal => None
                    case (name, ver) => Some(name -> ver.sym.nextDefinedVersion)
                }
            }
            
            val defines_loop = kind match {
                case SkLoopProper => make_auto_defines
                case _ => List()
            }
            
            val defines_auto = make_auto_defines
            
            /** Set to true if a cond or uncond break targets this scope. */
            var targetOfBreak = false
            
            // ___ Subsequences _____________________________________________________            
            
            def subseq[R](func: (MutableStmtSeq => R)) = {
                val msubseq = new MutableStmtSeq(symtab_in ++ defines_loop, this :: parents)
                val result = func(msubseq)
                addSeq(msubseq.toStmtSeq)
                result
            }
            
            // ___ Finalizing into a compound statement _____________________________            
            
            def toCompoundStmt = {
                val ckind = kind match {
                    case SkAnonBlock | SkLoopBreak | SkLoopContinue => 
                        assert(seqsBuffer.length == 1)
                        ir.Block(seqsBuffer(0))
                        
                    case SkSwitch | SkIf => 
                        ir.Switch(seqsBuffer.toList)
                        
                    case SkLoopProper => 
                        assert(seqsBuffer.length == 1)
                        
                        val args = defines_loop.map(_._2.toLvDecl)
                        
                        val lvs_initial = defines_loop.map { case (nm, _) =>
                            symtab_in(nm).lv
                        }
                        
                        ir.Loop(args, lvs_initial, seqsBuffer(0))
                        
                    case SkInlineInterval(x) => 
                        assert(seqsBuffer.length == 2)
                        ir.InlineInterval(x, seqsBuffer(0), seqsBuffer(1))
                        
                    case SkTryCatch =>
                        assert(seqsBuffer.length == 2)
                        ir.TryCatch(seqsBuffer(0), seqsBuffer(1))
                }
                
                val defines = defines_auto.map(_._2.toLvDecl) ++ defines_xtra
                
                ir.StmtCompound(ckind, defines)
            }

        }
        
        class MutableStmtSeq(symtab_in: SymbolTable, scopes: List[Scope]) {
            // ___ Statement List, Symbol Table _____________________________________

            private val stmtsBuffer = new ListBuffer[ir.Stmt]()            
            
            def addStmt(treePos: Tree, stmt: ir.Stmt) {
                stmtsBuffer += stmt.withPos(TreePosition(treePos, symtab.replaceFunc))
            }
            
            def toStmtSeq = ir.StmtSeq(stmtsBuffer.toList)
                
            var symtab = symtab_in
            
            // ___ Finalizing into a Compound Stmt __________________________________

            def subscope(
                tree: Tree,
                subkind: ScopeKind, 
                sublabel: Option[Name], 
                decls_xtra: ir.LvDecl*
            )(
                ctor_func: (Scope => Unit)
            ): Boolean = {
                val subscope = new Scope(subkind, scopes, sublabel, symtab, decls_xtra.toList)
                ctor_func(subscope)
                addStmt(tree, subscope.toCompoundStmt)
                symtab ++= subscope.defines_auto
                subscope.targetOfBreak
            }
            
            // ___ Break, Continue statements _______________________________________
            
            def branchTarget(branchKind: BranchKind, o_label: Option[Name]): Option[Scope] = {
                o_label match {
                    case None =>
                        scopes.find { scope =>
                            branchKind.anonTargets(scope.kind)
                        }
                    case Some(_) =>
                        scopes.find { scope =>
                            branchKind.namedTargets(scope.kind) && o_label == scope.o_label
                        }                    
                }
            }

            def branchArguments(idx: Int, lvs_xtra: ir.VarName*) = {
                val scope_tar = scopes(idx)
                assert(scope_tar.defines_xtra.length == lvs_xtra.length)                
                val lvs_auto = scope_tar.defines_auto.map { case (nm, _) =>
                    symtab(nm).lv
                }
                lvs_auto ++ lvs_xtra
            }
            
            def uncondBreak(treePos: Tree, scope: Scope, lvs_xtra: ir.VarName*) = {
                scope.targetOfBreak = true
                val idx = scopes.indexOf(scope)
                addStmt(treePos, ir.StmtBreak(idx, branchArguments(idx, lvs_xtra: _*)))
                false
            }
            
            def condBreak(treePos: Tree, scope: Scope, lvs_xtra: ir.VarName*) = {
                scope.targetOfBreak = true
                val idx = scopes.indexOf(scope)
                addStmt(treePos, ir.StmtCondBreak(idx, branchArguments(idx, lvs_xtra: _*)))
                true
            }
            
            def uncondContinue(treePos: Tree, scope: Scope) = {
                val idx = scopes.indexOf(scope)
                assert(scopes(idx).kind == SkLoopProper)
                addStmt(treePos, ir.StmtContinue(idx, branchArguments(idx)))
                false
            }            
            
            // ___ Misc. Helper Functions ___________________________________________

            def env(tree: Tree): ttf.TranslateEnv = {
                new ttf.TranslateEnv( // Only final variables may be named in types, etc:
                    TreePosition(tree, symtab.replaceFunc), 
                    symtab.foldLeft(env_mthd.m_localVariables) { 
                        case (m, (name, ver)) if ver.sym.isFinal =>
                            m + (name -> (ver.lv.path, ver.sym.annty.getUnderlyingType))
                        case (m, _) => m
                    },
                    env_mthd.m_defaultWghosts
                )
            }
                        
            def wtref(etree: ExpressionTree, wgs_default: List[ir.WcGhost]) = {
                ttf.wtref(env(etree), wgs_default)(ttf.getAnnotatedType(etree))
            }

            def call(treePos: Tree, lv_rcvr: ir.VarName, m: ir.MethodName, lvs_args: ir.VarName*) = {
                val lv_def = freshVar
                addStmt(treePos, ir.StmtCall(lv_def, lv_rcvr, m, lvs_args.toList))
                lv_def
            }

            def toString(etree: ExpressionTree) {
                val p = rvalue(etree)
                val annty = ttf.getAnnotatedType(etree)
                if(!annty.getKind.isPrimitive)
                    call(etree, p, methodName(wke.toStringEelem))
            }
            
            // Produces a fresh variable whose type is 'annty' (whose value is null)
            def nullStmt(tree: Tree, wt: ir.WcTypeRef): ir.VarName = {
                val lv_def = freshVar
                addStmt(tree, ir.StmtNull(lv_def, wt))
                lv_def
            }
            
            def nullStmt(etree: ExpressionTree): ir.VarName = {
                nullStmt(etree, wtref(etree, ir.wgs_constructed))
            }
            
            def load(treePos: Tree, lv_owner: ir.VarName, f: ir.FieldName) = {
                val lv_def = freshVar
                addStmt(treePos, ir.StmtGetField(lv_def, lv_owner, f))
                lv_def
            }
            
            def loadStatic(tree: Tree, telem: TypeElement) = {
                val cname = ttf.className(telem)
                val lv_def = freshVar
                addStmt(tree, ir.StmtGetStatic(lv_def, cname))
                lv_def                
            }
            
            /** Returns a local variable that owns `elem`, which must be some
              * member element of the current class accessed in an unqualified fashion.  
              * For example,  it might refer to a field (static or otherwise) or a
              * plain method call like `m(...)`. */
            def loadOwner(tree: Tree, elem: Element) = {
                if(EU.isStatic(elem)) {
                    loadStatic(tree, EU.enclosingClass(elem))
                } else {
                    // XXX Nested classes.
                    ir.lv_this
                }
            }

            def store(treePos: Tree, lv_owner: ir.VarName, f: ir.FieldName, lv_value: ir.VarName) = {
                addStmt(treePos, ir.StmtSetField(lv_owner, f, lv_value))
            }    
            
            // ___ Assignments ______________________________________________________
            //
            // Handles an assignment from etree_rval to etree_lval.

            def rvalueOrNull(
                etree_lval: ExpressionTree,
                oetree_rval: Option[ExpressionTree]
            ): ir.VarName = 
                oetree_rval match {
                    case None => nullStmt(etree_lval)
                    case Some(etree_rval) => rvalue(etree_rval)
                }

            def assign(
                tree: Tree, // location that caused the assignment
                etree_lval: ExpressionTree, // LHS
                oetree_rval: Option[ExpressionTree] // optional RHS (if None, treat as null)
            ): ir.VarName = at(etree_lval, symtab, "assign", nullStmt(etree_lval)) {
                // [1] I think that the correct result from an assignment is one
                // with the value of RHS and type of LHS.  However, I simply return
                // the value of the RHS, which means the type may be a subtype of the
                // "proper" one.  Returning the LHS as an rvalue could be more correct
                // but tends to lead to confusing double errors if the LHS is not 
                // writable (because it's usually not readable either!)
                etree_lval match {
                    case tree: ArrayAccessTree => // p[q] = r
                        val p = rvalue(tree.getExpression)
                        val q = rvalue(tree.getIndex)
                        val r = rvalueOrNull(etree_lval, oetree_rval)
                        call(tree, p, ir.m_arraySet, q, r)
                        r // See [1] above

                    case tree: IdentifierTree => 
                        val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                        val q = rvalueOrNull(etree_lval, oetree_rval)
                        elem.getKind match {
                            case EK.PARAMETER | EK.LOCAL_VARIABLE => // x = q (where x is a new version)
                                val sym = symtab(nm(elem)).sym
                                val ver = sym.nextExprVersion(q)
                                addStmt(tree, ir.StmtCheckType(q, sym.wtref))
                                symtab += Pair(nm(elem), ver)
                                ver.lv

                            case EK.FIELD => // [this.]f = q
                                val lv_owner = loadOwner(tree, elem)
                                store(tree, lv_owner, fieldName(elem), q)
                                q // See [1] above

                            case _ =>
                                throw new Unhandled(tree)

                        }

                    case tree: MemberSelectTree => // p.f = q
                        val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                        assert(elem.getKind == EK.FIELD)
                        val p = rvalue(tree.getExpression)
                        val q = rvalueOrNull(etree_lval, oetree_rval)
                        store(tree, p, fieldName(elem), q)
                        q // See [1] above

                    case tree =>
                        throw new Unhandled(tree)
                }
            }

            // ___ Method Invocations _______________________________________________
            
            def constructorInvocation(
                tree: NewClassTree,
                gs_default: List[ir.Ghost]
            ): ir.VarName = {
                val lv = freshVar
                val elem = IU.constructor(tree)
                val t = chkNoWcInTref(wtref(tree, List())).withDefaultGhosts(gs_default)
                val m = ttf.methodName(elem)
                val qs = tree.getArguments.map(rvalue).toList
                addStmt(tree, ir.StmtNew(lv, t, m, qs))
                lv                
            }
            
            def nonIntrinsicMethodInvocation(
                eelem: ExecutableElement,
                mitree: MethodInvocationTree
            ): ir.VarName = mitree.getMethodSelect match {
                case tree: IdentifierTree => // [this].m(...) or super.m(...)
                    val m = ttf.methodName(eelem)
                    tree.getName.toString match {
                        case "super" => 
                            eelem.getKind match {
                                case EK.CONSTRUCTOR => 
                                    val qs = mitree.getArguments.map(rvalue)
                                    addStmt(
                                        mitree, 
                                        ir.StmtSuperCtor(m, qs.toList))
                                    ir.lv_this // Dummy return value.
                                    
                                case _ => 
                                    val qs = mitree.getArguments.map(rvalue)
                                    val r = freshVar
                                    addStmt(
                                        mitree, 
                                        ir.StmtSuperCall(r, m, qs.toList))
                                    r
                            }

                        case _ =>
                            val lv_owner = loadOwner(tree, eelem)
                            val qs = mitree.getArguments.map(rvalue)
                            call(mitree, lv_owner, m, qs: _*)
                    }

                case tree: MemberSelectTree => // p.m(...)
                    val m = ttf.methodName(eelem)
                    val p = rvalue(tree.getExpression)
                    val qs = mitree.getArguments.map(rvalue)
                    call(mitree, p, m, qs: _*)

                case tree =>
                    throw new Unhandled(tree)     
            }
            
            /** Translate the argument to an `inline()` call during
              * `explicitInvocationsOfInitAndRun`.
              */
            def inlineTaskObj(
                lv_intervalName: ir.VarName,
                etree: ExpressionTree
            ) = etree match {
                case nctree: NewClassTree =>
                    constructorInvocation(nctree, List(
                        ir.Ghost(fieldName(wke.Subinterval.elem), lv_intervalName.path)))
                case _ => rvalue(etree)
            }
            
            /** Translate an inline interval call:
              *
              * Right now, we have special support for calls like:
              * `Intervals.inline(new SomeType())`.  We create an inline
              * interval in the IR and add it as the @Subinterval() ghost.
              */
            def inlineCall(
                mitree: MethodInvocationTree
            ): ir.VarName = {
                val eelem = TU.elementFromUse(mitree)
                val etree_arg = mitree.getArguments.get(0)
                val lv_intervalName = freshVar
                
                val (eelem_taskInit, eelem_taskRun) = {
                    if(eelem == wke.inlineVoid) (wke.voidInlineTaskInit, wke.voidInlineTaskRun)
                    else (wke.inlineTaskInit, wke.inlineTaskRun)
                }
                val (m_taskInit, m_taskRun) = (methodName(eelem_taskInit), methodName(eelem_taskRun))
                
                def explicitInvocationsOfInitAndRun = {
                    val wt_result = wtref(mitree, ir.wgs_constructed)
                    val decl = ir.LvDecl(freshVar, wt_result, List())

                    subscope(mitree, SkInlineInterval(lv_intervalName), None, decl) {
                        scope_interval =>
                            val lv_taskObj = scope_interval.subseq { seq_init =>
                                val lv_taskObj = seq_init.inlineTaskObj(lv_intervalName, etree_arg)
                                seq_init.call(mitree, lv_taskObj, m_taskInit, lv_intervalName)
                                lv_taskObj
                            }
                            
                            scope_interval.subseq { seq_run =>
                                val lv_ret = seq_run.call(mitree, lv_taskObj, m_taskRun, lv_intervalName)
                                seq_run.uncondBreak(mitree, scope_interval, lv_ret)
                            }
                    }
                    decl.name
                }
                
                TU.skipParens(etree_arg) match {
                    case nctree: NewClassTree =>
                        val ctorElem = TU.elementFromUse(nctree)
                        Option(nctree.getClassBody) match {
                            case Some(ctree) if ctorElem == wke.inlineTaskInit =>
                                explicitInvocationsOfInitAndRun
                            
                            case Some(ctree) if ctorElem == wke.voidInlineTaskInit =>
                                explicitInvocationsOfInitAndRun
                            
                            case _ =>
                                explicitInvocationsOfInitAndRun
                        }
                    
                    case _ =>
                        explicitInvocationsOfInitAndRun
                }
            }
            
            def methodInvocation(
                mitree: MethodInvocationTree
            ): ir.VarName = {
                val eelem = TU.elementFromUse(mitree)

                // First check for intrinsics:
                if(wke.addHb(eelem)) {
                    val qs = mitree.getArguments.map(rvalue)
                    addStmt(mitree, ir.StmtHb(qs(0), qs(1)))
                    ir.lv_this // Dummy return value.
                } else if(wke.inline(eelem)) {
                    inlineCall(mitree)
                } else {
                    nonIntrinsicMethodInvocation(eelem, mitree)
                }            
            }

            // ___ Rvalue: Evaluating an expression to a path _______________________

            def rvalue(
                etree: ExpressionTree
            ): ir.VarName = at(etree, symtab, "rvalue", nullStmt(etree)) {
                etree match {
                    case tree: ArrayAccessTree => // p[q]                
                        val p = rvalue(tree.getExpression)
                        val q = rvalue(tree.getIndex)
                        call(tree, p, ir.m_arrayGet, q)

                    case tree: AssignmentTree => // lvalue = q
                        assign(tree, tree.getVariable, Some(tree.getExpression))

                    case tree: BinaryTree => // p + q (or some other operator)
                        tree.getKind match {
                            case TRK.EQUAL_TO | TRK.NOT_EQUAL_TO =>
                                rvalue(tree.getLeftOperand)
                                rvalue(tree.getRightOperand)
                            
                            case _ =>
                                toString(tree.getLeftOperand)
                                toString(tree.getRightOperand)                                
                        }
                        nullStmt(tree)

                    case tree: CompoundAssignmentTree => // p += q
                        toString(tree.getVariable)
                        toString(tree.getExpression)
                        assign(tree, tree.getVariable, None)

                    case tree: ConditionalExpressionTree => // p = (cond ? q : r)
                        rvalue(tree.getCondition) // we don't really need to use the condition
                        
                        // Result variable:
                        val lv_res = freshVar
                        val wt_res = wtref(etree, ir.wgs_constructed)
                        
                        subscope(tree, SkIf, None, ir.LvDecl(lv_res, wt_res, List())) { 
                            scope_sw =>
                                scope_sw.subseq { seq_t =>
                                    val p_t = seq_t.rvalue(tree.getTrueExpression)                        
                                    seq_t.uncondBreak(tree, scope_sw, p_t)
                                }
                            
                                scope_sw.subseq { seq_f =>
                                    val p_f = seq_f.rvalue(tree.getFalseExpression)
                                    seq_f.uncondBreak(tree, scope_sw, p_f)                                
                                }                            
                        }
                        
                        lv_res

                    case tree: IdentifierTree => 
                        val elem = TU.elementFromUse(tree)
                        val nm_elem = nm(elem)
                        if(nm_elem == "this")
                            ir.lv_this // TODO: Inner classes also use qualified forms of this
                        else elem.getKind match {
                            case EK.PARAMETER | EK.LOCAL_VARIABLE => symtab(nm_elem).lv
                            case k if k.isField =>
                                val lv_owner = loadOwner(tree, elem)
                                load(tree, lv_owner, fieldName(elem))
                            case EK.METHOD => ir.lv_this // happens for "implicit this" calls like foo(...)
                            case k if k.isClass || k.isInterface =>
                                loadStatic(tree, elem.asInstanceOf[TypeElement])
                            case _ => throw new Unhandled(tree)
                        }

                    case tree: LiteralTree =>
                        nullStmt(tree)

                    case tree: MemberSelectTree =>
                        val elem = TU.elementFromUse(tree)
                        elem.getKind match {
                            case k if k.isField =>  // p.f
                                val p = rvalue(tree.getExpression)
                                load(tree, p, fieldName(elem))
                            case k if k.isClass || k.isInterface =>
                                loadStatic(tree, elem.asInstanceOf[TypeElement])
                            case EK.METHOD => // p.m, just evaluate receiver
                                rvalue(tree.getExpression)
                            case _ => throw new Unhandled(tree)
                        }

                    case tree: MethodInvocationTree => // p.m(Q)
                        methodInvocation(tree)

                    case tree: NewArrayTree => // p = new T[...]
                        // We don't really care about the initializers per se,
                        // just want to be sure they can be evaluated:
                        if(tree.getInitializers != null) {
                            for(initializer <- tree.getInitializers) {
                                rvalue(initializer)
                            }                            
                        }
                        
                        // Since arrays don't have constructors etc, just make sure
                        // we create a variable with the right TYPE:
                        nullStmt(tree)
                        
                    case tree: NewClassTree => // p = new T(Q)
                        constructorInvocation(tree, List())
                        
                    case tree: ParenthesizedTree =>
                        rvalue(tree.getExpression)

                    case tree: UnaryTree => // !p, p++, p-- etc
                        assert(ttf.getAnnotatedType(tree).getKind.isPrimitive)
                        tree.getKind match {
                            // p++, p-- are effectively writes:
                            case TRK.POSTFIX_DECREMENT | TRK.POSTFIX_INCREMENT | TRK.PREFIX_DECREMENT | TRK.PREFIX_INCREMENT =>
                                assign(tree, tree.getExpression, None)
                            case _ => 
                                nullStmt(tree)
                        }

                    case _ =>
                        throw new Unhandled(etree)
                } 
            }
            
            // ___ Statements and Control-Flow ______________________________________
            
            /** Translates the various kinds of loops.  We use a structure like so:
              *
              *     LoopBreak
              *        <initializers>
              *        LoopProper
              *            <break from LoopBreak if pre-condition not met>
              *            LoopContinue
              *                <loop body>
              *            <break from LoopBreak if post-condition not met>
              *            <updates>
              *            <continue with LoopProper>
              * 
              * Note that any "continues" from the user are translated to
              * a *break* out of the LoopContinue scope (which then performs
              * any post-condition test and updates and re-iterates). */
            def loopStmt(
                o_loopLabel: Option[Name],
                tree: StatementTree,
                body: StatementTree,
                o_preCondition: Option[ExpressionTree] = None,
                o_postCondition: Option[ExpressionTree] = None,
                initializers: Iterable[StatementTree] = List(),
                updates: Iterable[StatementTree] = List()
            ): Boolean = {
                subscope(tree, SkLoopBreak, o_loopLabel) { scope_break =>
                    scope_break.subseq { seq_break =>
                        if(seq_break.stmts(initializers)) {
                                seq_break.subscope(tree, SkLoopProper, None) { scope_loop =>
                                    scope_loop.subseq { seq_loop =>

                                        o_preCondition.foreach { condition =>
                                            seq_loop.rvalue(condition)
                                            seq_loop.condBreak(tree, scope_break)                                    
                                        }

                                        seq_loop.subscope(tree, SkLoopContinue, o_loopLabel) { scope_iter =>
                                            scope_iter.subseq { seq_iter =>
                                                if(seq_iter.stmt(None, body))
                                                    seq_iter.uncondBreak(tree, scope_iter)
                                            }
                                        }

                                        o_postCondition.foreach { condition =>
                                            seq_loop.rvalue(condition)
                                            seq_loop.condBreak(tree, scope_break)                                    
                                        }

                                        if(seq_loop.stmts(updates))
                                            seq_loop.uncondContinue(tree, scope_loop)                                        
                                    }
                                }
                            }                            
                        }
                }
            }
            
            /** Processes each statement in turn, returning true if control-flow
              * continues after all of them. */
            def stmts(lst: Iterable[StatementTree]) = {
                lst.foldLeft(true) { 
                    case (true, s) => stmt(None, s)
                    case (false, _) => false
                }                
            }
            
            /** Processes `tree` producing one or more statements.
              * Returns true if the control-flow continues afterwards. */
            def stmt(
                label_tree: Option[Name],
                tree: StatementTree
            ): Boolean = at(tree, symtab, "stmt", true) { 
                tree match {
                    case null => 
                        // in practice, null statements are always optional code where we
                        // don't want to generate anything.
                        true
                    
                    case tree: AssertTree =>
                        rvalue(tree.getCondition)
                        true

                    case tree: BlockTree =>
                        subscope(tree, SkAnonBlock, label_tree) { scope_blk =>
                            scope_blk.subseq { seq =>
                                if(stmts(tree.getStatements))
                                    seq.uncondBreak(tree, scope_blk)
                            }
                        }

                    case tree: BreakTree =>
                        branchTarget(BranchBreak, Option(tree.getLabel)) match {
                            case Some(scope) =>
                                uncondBreak(tree, scope)
                            case None =>
                                throw new CheckFailure(
                                    "intervals.internal.error",
                                    "Could not find target for break %s".format(tree)
                                )
                        }

                    case tree: ClassTree => 
                        throw new Unhandled(tree) // XXX inner classes

                    case tree: ContinueTree =>
                        branchTarget(BranchContinue, Option(tree.getLabel)) match {
                            case Some(scope) =>
                                // n.b.: User continues are translated to *breaks* from an inner scope.
                                uncondBreak(tree, scope) 
                            case None =>
                                throw new CheckFailure(
                                    "intervals.internal.error",
                                    "Could not find target for continue %s in scopes %s".format(tree, scopes)
                                )
                        }

                    case tree: DoWhileLoopTree =>
                        loopStmt(label_tree, tree,
                            o_postCondition = Some(tree.getCondition),
                            body = tree.getStatement
                        )
                        
                    case tree: EmptyStatementTree => true

                    case tree: EnhancedForLoopTree => throw new Unhandled(tree) // XXX enhanced for loops

                    case tree: ExpressionStatementTree => rvalue(tree.getExpression); true

                    case tree: ForLoopTree =>
                        loopStmt(label_tree, tree,
                            initializers = tree.getInitializer,
                            o_preCondition = Some(tree.getCondition),
                            updates = tree.getUpdate,
                            body = tree.getStatement
                        )

                    case tree: IfTree =>
                        rvalue(tree.getCondition)
                        subscope(tree, SkIf, label_tree) { scope =>
                            scope.subseq { seq =>
                                if(seq.stmt(None, tree.getThenStatement))
                                    seq.uncondBreak(tree, scope)
                            }
                            
                            scope.subseq { seq =>
                                if(seq.stmt(None, tree.getElseStatement))
                                    seq.uncondBreak(tree, scope)
                            }
                        }

                    case tree: LabeledStatementTree =>
                        stmt(Some(tree.getLabel), tree.getStatement)
                        
                    case tree: ReturnTree =>
                        addStmt(
                            tree, 
                            ir.StmtReturn(
                                Option(tree.getExpression).map(rvalue)))
                        false

                    case tree: VariableTree =>
                        val sym = createSymbol(env(tree))(tree)
                        val p = tree.getInitializer match {
                            case null => nullStmt(tree, sym.wtref)
                            case expr => rvalue(expr)
                        }
                        val ver = sym.nextExprVersion(p)
                        symtab += Pair(sym.name, ver)
                        true

                    case tree: WhileLoopTree =>
                        loopStmt(label_tree, tree,
                            o_preCondition = Some(tree.getCondition),
                            body = tree.getStatement
                        )

                    case _ => throw new Unhandled(tree)
                }
            }            
        }
        
        // ___ Building the method body _________________________________________
        
        val msubseq = new MutableStmtSeq(symtab_mthdParam, List())
        msubseq.stmt(None, mtree.getBody)
        msubseq.toStmtSeq
    }

}