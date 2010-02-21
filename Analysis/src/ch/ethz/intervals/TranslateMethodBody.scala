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
        import ttf.getAnnotatedType
        import ttf.fieldName
        import ttf.wke

        // ___ Miscellany _______________________________________________________

        class Unhandled(val tree: Tree) 
        extends RuntimeException("Could not handle: "+tree.getKind+" "+tree)

        var unique = 0
        def freshName(prefix: String) = {
            unique += 1
            "[%s/%d]".format(prefix, unique)
        }
        def freshVar() = ir.VarName(freshName("jv"))
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

        sealed class Symbol(val name: String, val annty: AnnotatedTypeMirror, val wtref: ir.WcTypeRef) {
            var maxVersion = 0
            
            // Version of the symbol provided as a method parameter.
            def globalVersion =
                GlobalVersion(this, ir.VarName(name))

            // Version of the symbol provided as a block parameter.
            def nextLocalVersion = {
                maxVersion += 1
                LocalVersion(this, maxVersion)
            }

            // Version of the symbol resulting form an assignment.
            def nextExprVersion(lv: ir.VarName) =
                ExprVersion(this, lv)

            override def toString = "Symbol(%s)".format(name)
        }

        abstract class Version {
            val sym: Symbol
            val lv: ir.VarName
            val isLocal: Boolean
        }    
        sealed case class GlobalVersion(sym: Symbol, lv: ir.VarName) extends Version {
            val isLocal = false
        }
        sealed case class LocalVersion(sym: Symbol, ver: Int) extends Version {
            val lv = ir.VarName(sym.name + "[" + ver + "]")
            val isLocal = true
            def toLvDecl = ir.LvDecl(lv, sym.wtref)
        }
        sealed case class ExprVersion(sym: Symbol, lv: ir.VarName) extends Version {
            val isLocal = true
        }
        
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
            val wtref = ttf.wtref(env)(annty)
            new Symbol(nm(elem), annty, wtref)
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
        val vers_mthdParam = syms_mthdParam.map(_.globalVersion)
        val symtab_mthdParam = SymbolTable(elems_param.map(nm), vers_mthdParam)

        // ___ Scopes ___________________________________________________________
        
        abstract class ScopeKind
        case object ScopeKindBlock extends ScopeKind
        case object ScopeKindSwitch extends ScopeKind
        case object ScopeKindLoop extends ScopeKind
        case class ScopeKindSubinterval(x: ir.VarName, lvs_locks: List[ir.VarName]) extends ScopeKind
        case object ScopeKindTryCatch extends ScopeKind
                
        abstract class BranchKind(val targets: Set[ScopeKind])
        case object BranchBreak extends BranchKind(Set(ScopeKindBlock, ScopeKindSwitch))
        case object BranchContinue extends BranchKind(Set(ScopeKindLoop))
        
        class Scope(
            val kind: ScopeKind,
            val parents: List[Scope],
            val label: Option[Name],
            val symtab_in: SymbolTable,
            val defines_xtra: List[ir.LvDecl]
        ) {
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
            // Xtra definesare special purpose, anonymous defines 
            // such as the result of a (c ? t : f) expression.
            
            def make_auto_defines = {
                symtab_in.toList.map { case (name, ver0) =>
                    (name, ver0.sym.nextLocalVersion)
                }
            }
            
            val defines_loop = kind match {
                case ScopeKindLoop => make_auto_defines
                case _ => List()
            }
            
            val defines_auto = make_auto_defines
            
            // ___ Subsequences _____________________________________________________            
            
            def subseq(func: (MutableStmtSeq => Unit)) {
                val msubseq = new MutableStmtSeq(symtab_in, this :: parents)
                func(msubseq)
                addSeq(msubseq.toStmtSeq)
            }
            
            // ___ Finalizing into a compound statement _____________________________            
            
            def toCompoundStmt = {
                val ckind = kind match {
                    case ScopeKindBlock => 
                        assert(seqsBuffer.length == 1)
                        ir.Block(seqsBuffer(0))
                        
                    case ScopeKindSwitch => 
                        ir.Switch(seqsBuffer.toList)
                        
                    case ScopeKindLoop => 
                        assert(seqsBuffer.length == 1)
                        
                        val args = defines_loop.map(_._2.toLvDecl)
                        
                        val lvs_initial = defines_loop.map { case (nm, _) =>
                            symtab_in(nm).lv
                        }
                        
                        ir.Loop(args, lvs_initial, seqsBuffer(0))
                        
                    case ScopeKindSubinterval(x, ps_locks) => 
                        assert(seqsBuffer.length == 1)
                        ir.Subinterval(x, ps_locks, seqsBuffer(0))
                        
                    case ScopeKindTryCatch =>
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
            ) = {
                val subscope = new Scope(subkind, scopes, sublabel, symtab, decls_xtra.toList)
                ctor_func(subscope)
                addStmt(tree, subscope.toCompoundStmt)
                symtab ++= subscope.defines_auto
            }
            
            // ___ Break, Continue statements _______________________________________
            
            def branchTarget(branchKind: BranchKind, olabel: Option[Name]): Option[Scope] = {
                scopes.find { scope =>
                    branchKind.targets(scope.kind) &&
                    (olabel == None || olabel == scope.label)
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
            
            def uncondBreak(treePos: Tree, scope: Scope, lvs_xtra: ir.VarName*) {
                val idx = scopes.indexOf(scope)
                addStmt(treePos, ir.StmtBreak(idx, branchArguments(idx, lvs_xtra: _*)))
            }
            
            def condBreak(treePos: Tree, scope: Scope, lvs_xtra: ir.VarName*) {
                val idx = scopes.indexOf(scope)
                addStmt(treePos, ir.StmtCondBreak(idx, branchArguments(idx, lvs_xtra: _*)))
            }
            
            def uncondContinue(treePos: Tree, scope: Scope) {
                val idx = scopes.indexOf(scope)
                addStmt(treePos, ir.StmtContinue(idx, branchArguments(idx)))
            }            
            
            // ___ Misc. Helper Functions ___________________________________________

            def env(tree: Tree): ttf.TranslateEnv = {
                new ttf.TranslateEnv(
                    TreePosition(tree, symtab.replaceFunc), 
                    symtab.foldLeft(env_mthd.m_localVariables) { case (m, (name, ver)) =>
                        m + (name -> (ver.lv.path, ver.sym.annty.getUnderlyingType))
                    },
                    env_mthd.m_defaultWghosts
                )
            }
                        
            def wtref(etree: ExpressionTree) = {
                ttf.wtref(env(etree))(getAnnotatedType(etree))
            }

            def call(treePos: Tree, lv_rcvr: ir.VarName, m: ir.MethodName, lvs_args: ir.VarName*) = {
                val lv_def = freshVar
                addStmt(treePos, ir.StmtCall(lv_def, lv_rcvr, m, lvs_args.toList))
                lv_def
            }

            def toString(etree: ExpressionTree) {
                val p = rvalue(etree)
                val annty = getAnnotatedType(etree)
                if(!annty.getKind.isPrimitive)
                    call(etree, p, ir.m_toString)
            }
            
            // Produces a fresh variable whose type is 'annty' (whose value is null)
            def nullStmt(tree: Tree, wt: ir.WcTypeRef): ir.VarName = {
                val lv_def = freshVar
                addStmt(tree, ir.StmtNull(lv_def, wt))
                lv_def
            }
            
            def nullStmt(etree: ExpressionTree): ir.VarName = {
                nullStmt(etree, wtref(etree))
            }
            
            def load(treePos: Tree, lv_owner: ir.VarName, f: ir.FieldName) = {
                val lv_def = freshVar
                addStmt(treePos, ir.StmtGetField(lv_def, lv_owner, f))
                lv_def
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
                                store(tree, ir.lv_this, fieldName(elem), q)
                                q // See [1] above

                            case _ =>
                                throw new Unhandled(tree)

                        }

                    case tree: MemberSelectTree => // p.f = q
                        val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                        assert(elem.getKind == EK.FIELD)
                        val p = rvalueIfNotStatic(elem, tree.getExpression)
                        val q = rvalueOrNull(etree_lval, oetree_rval)
                        store(tree, p, fieldName(elem), q)
                        q // See [1] above

                    case tree =>
                        throw new Unhandled(tree)
                }
            }

            // ___ Method Invocations _______________________________________________
            
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
                            // XXX Nested classes can use a different this.
                            val qs = mitree.getArguments.map(rvalue)
                            call(mitree, ir.lv_this, m, qs: _*)
                    }

                case tree: MemberSelectTree => // p.m(...)
                    val m = ttf.methodName(eelem)
                    val p = rvalueIfNotStatic(eelem, tree.getExpression)
                    val qs = mitree.getArguments.map(rvalue)
                    call(mitree, p, m, qs: _*)

                case tree =>
                    throw new Unhandled(tree)     
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
                } else {
                    nonIntrinsicMethodInvocation(eelem, mitree)
                }            
            }

            // ___ Rvalue: Evaluating an expression to a path _______________________

            def rvalueIfNotStatic(elem: Element, etree: ExpressionTree): ir.VarName =
                if(EU.isStatic(elem)) throw new Unhandled(etree) // XXX static
                else rvalue(etree)
                
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
                        toString(tree.getLeftOperand)
                        toString(tree.getRightOperand)
                        nullStmt(tree)

                    case tree: CompoundAssignmentTree => // p += q
                        toString(tree.getVariable)
                        toString(tree.getExpression)
                        assign(tree, tree.getVariable, None)

                    case tree: ConditionalExpressionTree => // p = (cond ? q : r)
                        rvalue(tree.getCondition) // we don't really need to use the condition
                        
                        // Result variable:
                        val lv_res = freshVar
                        val wt_res = wtref(etree)
                        
                        subscope(tree, ScopeKindSwitch, None, ir.LvDecl(lv_res, wt_res)) { scope_sw =>
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
                            case EK.FIELD => load(tree, ir.lv_this, fieldName(elem))
                            case EK.METHOD => ir.lv_this // happens for "implicit this" calls like foo(...)
                            case EK.CONSTRUCTOR => throw new Unhandled(tree)
                            case _ => throw new Unhandled(tree)
                        }

                    case tree: LiteralTree =>
                        nullStmt(tree)

                    case tree: MemberSelectTree =>
                        val elem = TU.elementFromUse(tree)
                        if(elem.getKind.isField) { // p.f
                            val p = rvalue(tree.getExpression)
                            load(tree, p, fieldName(elem))
                        } else { // p.m, just evaluate receiver
                            rvalue(tree.getExpression)
                        }

                    case tree: MethodInvocationTree => // p.m(Q)
                        methodInvocation(tree)

                    case tree: NewClassTree => // p = new T(Q)
                        val lv = freshVar
                        val elem = IU.constructor(tree)
                        val t = chkNoWcInTref(wtref(tree))
                        val m = ttf.methodName(elem)
                        val qs = tree.getArguments.map(rvalue).toList
                        addStmt(tree, ir.StmtNew(lv, t, m, qs))
                        lv
                        
                    case tree: ParenthesizedTree =>
                        rvalue(tree.getExpression)

                    case tree: UnaryTree => // !p, p++, p-- etc
                        assert(getAnnotatedType(tree).getKind.isPrimitive)
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
            
            def stmt(
                label_tree: Option[Name],
                tree: StatementTree
            ): Unit = at(tree, symtab, "stmt", ()) { 
                tree match {
                    case tree: AssertTree =>
                        rvalue(tree.getCondition)

                    case tree: BlockTree =>
                        subscope(tree, ScopeKindBlock, label_tree) { scope_blk =>
                            scope_blk.subseq { seq =>
                                tree.getStatements.foreach(seq.stmt(None, _))                                
                                seq.uncondBreak(tree, scope_blk)                        
                            }
                        }

                    case tree: BreakTree =>
                        branchTarget(BranchBreak, nullToOption(tree.getLabel)) match {
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
                        branchTarget(BranchContinue, nullToOption(tree.getLabel)) match {
                            case Some(scope) =>
                                uncondContinue(tree, scope)
                            case None =>
                                throw new CheckFailure(
                                    "intervals.internal.error",
                                    "Could not find target for continue %s".format(tree)
                                )
                        }

                    case tree: DoWhileLoopTree =>
                        // The "outer" scope is the target for breaks, 
                        // "inner" is the target for continues.
                        subscope(tree, ScopeKindBlock, label_tree) { scope_outer =>
                            scope_outer.subseq { seq_outer =>
                                seq_outer.subscope(tree, ScopeKindLoop, label_tree) { scope_inner =>
                                    scope_inner.subseq { seq_inner =>
                                        seq_inner.stmt(None, tree.getStatement)
                                        seq_inner.rvalue(tree.getCondition)
                                        seq_inner.condBreak(tree, scope_outer)
                                        seq_inner.uncondContinue(tree, scope_inner)                            
                                    }
                                }                                
                            }
                        }
                    case tree: EmptyStatementTree => ()

                    case tree: EnhancedForLoopTree => throw new Unhandled(tree) // XXX enhanced for loops

                    case tree: ExpressionStatementTree => rvalue(tree.getExpression)

                    case tree: ForLoopTree =>
                        // Rather involved transformation:
                        //  outer: Block {
                        //      <initializers>
                        //      <condition>
                        //      condBreak outer;
                        //      <statement>
                        //      inner: Loop {
                        //          <update>
                        //          <condition>
                        //          condBreak outer;
                        //          <statement>
                        //          continue inner;
                        //      }
                        //  }

                        subscope(tree, ScopeKindBlock, label_tree) { scope_outer =>
                            scope_outer.subseq { seq_outer =>
                                tree.getInitializer.foreach(seq_outer.stmt(None, _))
                                seq_outer.rvalue(tree.getCondition)
                                seq_outer.condBreak(tree, scope_outer)
                                seq_outer.stmt(None, tree.getStatement)                        

                                seq_outer.subscope(tree, ScopeKindLoop, label_tree) { scope_inner =>
                                    scope_inner.subseq { seq_inner =>
                                        tree.getUpdate.foreach(seq_inner.stmt(None, _))
                                        seq_inner.rvalue(tree.getCondition)
                                        seq_inner.condBreak(tree, scope_outer)
                                        seq_inner.stmt(None, tree.getStatement)
                                        seq_inner.uncondContinue(tree, scope_inner)                                        
                                    }
                                }                                
                            }
                        }

                    case tree: LabeledStatementTree =>
                        stmt(Some(tree.getLabel), tree.getStatement)

                    case tree: VariableTree =>
                        val sym = createSymbol(env(tree))(tree)
                        val p = tree.getInitializer match {
                            case null => nullStmt(tree, sym.wtref)
                            case expr => rvalue(expr)
                        }
                        val ver = sym.nextExprVersion(p)
                        symtab += Pair(sym.name, ver)

                    case tree: WhileLoopTree =>
                        // The "outer" scope is the target for breaks, 
                        // "inner" is the target for continues.
                        subscope(tree, ScopeKindBlock, label_tree) { scope_outer =>
                            scope_outer.subseq { seq_outer =>
                                seq_outer.subscope(tree, ScopeKindLoop, label_tree) { scope_inner =>
                                    scope_inner.subseq { seq_inner =>
                                        seq_inner.rvalue(tree.getCondition)
                                        seq_inner.condBreak(tree, scope_outer)
                                        seq_inner.stmt(None, tree.getStatement)
                                        seq_inner.uncondContinue(tree, scope_inner)                            
                                    }
                                }                                
                            }
                        }

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