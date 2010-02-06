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
import scala.collection.jcl.Conversions._
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
import quals.DefinesGhost
import ch.ethz.intervals.quals._
import ch.ethz.intervals.log.LogStack

object TranslateMethodBody
{
    def apply(
        logStack: LogStack, 
        processingEnvironment: ProcessingEnvironment,
        ttf: TranslateTypeFactory, 
        mtree: MethodTree
    ): Array[ir.Block] = logStack.log.indentedRes("translateMethodBody(%s)", mtree) {
        import logStack.log
        import ttf.TreePosition
        import ttf.DummyPositional
        import ttf.DummyPosition
        import ttf.getAnnotatedType
        import ttf.f

        // ___ Miscellany _______________________________________________________

        class Unhandled(val tree: Tree) 
        extends RuntimeException("Could not handle: "+tree.getKind+" "+tree)

        var unique = 0
        def freshName(prefix: String) = {
            unique += 1
            prefix + "[" + unique + "]"
        }
        def freshVar() = ir.VarName(freshName("jv"))
        def nm(elem: Element) = elem.getSimpleName.toString

        def chkPath(wp: ir.WcPath) = wp match {
            case p: ir.Path => p
            case _ => throw new CheckFailure("intervals.wildcard.not.perm", wp)
        }

        def chkGhost(wg: ir.WcGhost) = 
            ir.Ghost(wg.f, chkPath(wg.wp))

        def chkTref(wt: ir.WcTypeRef): ir.TypeRef =
            ir.TypeRef(wt.c, wt.wghosts.map(chkGhost), wt.as)

        def at[R](treePos: Tree, tag: String, defFunc: => R)(func: => R) =
            logStack.at(new DummyPositional(TreePosition(treePos), tag), defFunc)(func)

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
            def nextExprVersion(p: ir.Path) =
                ExprVersion(this, p)

            override def toString = "Symbol(%s)".format(name)
        }

        abstract class Version {
            val sym: Symbol
            val p: ir.Path        
            val isLocal: Boolean
        }    
        sealed case class GlobalVersion(sym: Symbol, lv: ir.VarName) extends Version {
            val p = lv.path
            val isLocal = false
        }
        sealed case class LocalVersion(sym: Symbol, ver: Int) extends Version {
            val lv = ir.VarName(sym.name + "[" + ver + "]")
            val p = lv.path        
            val isLocal = true
            def toLvDecl = ir.LvDecl(lv, sym.wtref)
        }
        sealed case class ExprVersion(sym: Symbol, p: ir.Path) extends Version {
            val isLocal = true
        }

        type SymbolTable = Map[String, Version]
        def SymbolTable(names: List[String], vers: List[Version]): SymbolTable =
            Map(names.zip(vers): _*)

        def createSymbol(env: ttf.TranslateEnv)(vtree: VariableTree): Symbol = {
            val elem = TU.elementFromDeclaration(vtree)
            val annty = ttf.getAnnotatedType(elem)
            val wtref = ttf.wtref(env)(annty)
            new Symbol(nm(elem), annty, wtref)
        }

        // ___ Well-known methods _______________________________________________
        val wke = new WellKnownElements(
            processingEnvironment.getElementUtils, 
            processingEnvironment.getTypeUtils)
        
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
        case object ScopeKindSeq extends ScopeKind
        case object ScopeKindSwitch extends ScopeKind
        case object ScopeKindLoop extends ScopeKind
        case class ScopeKindSubinterval(x: VarName, ps_locks: List[Path]) extends ScopeKind
        case object ScopeKindTryCatch extends ScopeKind
                
        abstract class BranchKind(val targets: Set[ScopeKind])
        case object BranchBreak extends BranchKind(Set(ScopeSeq, ScopeSwitch))
        case object BranchContinue extends BranchKind(Set(ScopeLoop))
        
        class Scope(
            val kind: ScopeKind
            val prev: Option[Scope]
            val label: Option[Name]
            val symtab_in: SymbolTable
            val defines_xtra: List[LvDecl]
        ) {
            // ___ Statement List ___________________________________________________

            private val stmtsBuffer = new ListBuffer[ir.Stmt]()            
            
            def addStmt(treePos: Tree, stmt: ir.Stmt) =
                stmtsBuffer += stmt.withPos(TreePosition(treePos))
                
            // ___ Counting up the stack ____________________________________________
            
            def countUp(idx: Integer): Scope = {
                if(idx == 0) 
                    this
                else 
                    prev.get.countUp(idx - 1)
            }
            
            def branchIndex(branchKind: BranchKind, olabel: Option[Name]) = {
                if(branchKind.targets(kind) && (olabel == None || olabel == label)) {
                    0 
                } else {
                    1 + prev.get.branchIndex(branchKind, olabel)
                }                
            }
            
            // ___ Symbol Table and Flow of Control _________________________________
            //            
            // Defines are the variables defined on exit from this scope.  
            //
            // Auto defines are user-declared variables, whose most 
            // recent version is automatically carried from scope-to-scope.  
            //
            // Xtra definesare special purpose, anonymous defines 
            // such as the result of a (c ? t : f) expression.
            
            var symtab = symtab_in
            
            def make_auto_defines = {
                val buffer = new ListBuffer[(String, LvDecl)]()
                for((name, ver0) <- symtab_in) {
                    val ver1 = ver0.sym.nextLocalVersion
                    buffer += (name, ver1)
                }
                buffer.toList
            }
            
            val defines_loop = kind match {
                case ScopeKindLoop => make_auto_defines
                case _ => List()
            }
            
            val defines_auto = make_auto_defines
            
            def subscope(subkind: ScopeKind, sublabel: Option[Name], subxtra: ir.LvDecl*) = {
                val res = new Scope(subkind, Some(this), sublabel, symtab, sublabel.toList)
                symtab ++= res.defines_auto
            }
            
            // ___ Finalizing into a Compound Stmt __________________________________

            def toCompoundStmt = {
                val ckind = kind match {
                    case ScopeKindSeq => 
                        ir.Seq(stmtsBuffer.toList)
                        
                    case ScopeKindSwitch => 
                        ir.Switch(stmtsBuffer.toList)
                        
                    case ScopeKindLoop => 
                        assert(stmtsBuffer.length == 1)
                        
                        val args = defines_loop.map(_._2.toLvDecl)
                        
                        val ps_initial = defines_loop.map { case (nm, _) =>
                            symtab_in(nm).p
                        }
                        
                        ir.Loop(args, ps_initial, stmtsBuffer(0))
                        
                    case ScopeKindSubinterval(x, ps_locks) => 
                        assert(stmtsBuffer.length == 1)
                        ir.Subinterval(x, ps_locks, stmtsBuffer(0))
                        
                    case ScopeKindTryCatch =>
                        assert(stmtsBuffer.length == 2)
                        ir.TryCatch(stmtsBuffer(0), stmtsBuffer(1))
                }
                
                val defines = defines_auto.map(_._2.toLvDecl) ++ defines_xtra
                
                ir.StmtCompound(ckind, defines)
            }

            def subscope(
                subkind: ScopeKind, 
                sublabel: Option[Name], 
                decls_xtra: ir.LvDecl*
            )(
                ctor_func: (Scope => Unit)
            ) = {
                val subscope = new Scope(subkind, Some(this), sublabel, symtab, decls_xtra.toList)
                ctor_func(subscope)
                addStmt(tree, subscope.toCompoundStmt)
                symtab ++= subscope.defines_auto
            }
            
            // ___ Break, Continue statements _______________________________________
            
            def branchArguments(idx: Int, ps_xtra: ir.Path*) = {
                val scope_tar = countUp(idx)
                assert(scope_tar.defines_xtra.length == ps_xtra.length)                
                val ps_auto = scope_tar.defines_auto.map { case (nm, _) =>
                    symtab(nm).p
                }
                ps_auto ++ ps_xtra
            }
            
            def uncondBreak(treePos: Tree, idx: Int, ps_xtra: ir.Path*) {
                addStmt(treePos, ir.StmtBreak(idx, branchArguments(idx, ps_xtra)))
            }
            
            def condBreak(treePos: Tree, idx: Int, ps_xtra: ir.Path*) {
                addStmt(treePos, ir.StmtCondBreak(idx, branchArguments(idx, ps_xtra)))
            }
            
            def uncondContinue(treePos: Tree, idx: Int) {
                addStmt(treePos, ir.StmtContinue(idx, branchArguments(idx, List())))
            }            
            
            // ___ Misc. Helper Functions ___________________________________________

            def env(tree: Tree): ttf.TranslateEnv = {
                val m_lvs = symtab.foldLeft(env_mthd.m_lvs) { case (m, (name, ver)) =>
                    m + Pair(name, (ver.p, ver.sym.annty))
                }
                new ttf.TranslateEnv(TreePosition(tree), m_lvs, env_mthd.m_defaultWghosts)
            }
                        
            def wtref(etree: ExpressionTree) = {
                ttf.wtref(env(etree))(getAnnotatedType(etree))
            }

            def call(treePos: Tree, p: ir.Path, m: ir.MethodName, qs: ir.Path*) = {
                val r = freshVar
                addStmt(treePos, ir.StmtCall(r, p, m, qs.toList))
                r.path
            }

            def callUnit(treePos: Tree, p: ir.Path, m: ir.MethodName, qs: ir.Path*) {
                call(treePos, p, m, qs: _*)
            }

            def toString(etree: ExpressionTree) {
                val p = rvalue(this, etree)
                val annty = getAnnotatedType(etree)
                if(!annty.getKind.isPrimitive)
                    callUnit(etree, p, ir.m_toString)
            }
            
            def nullStmt(etree: ExpressionTree) = {
                val r = freshVar
                addStmt(etree, ir.StmtNull(r, wtref(etree)))
                r.path
            }
            
            def load(treePos: Tree, p: ir.Path, f: ir.FieldName) = {
                val r = freshVar
                addStmt(treePos, ir.StmtGetField(r, p, f))
                r.path
            }

            def store(treePos: Tree, p: ir.Path, f: ir.FieldName, q: ir.Path) = {
                addStmt(treePos, ir.StmtSetField(p, f, q))
            }    
            
            // ___ Assignments ______________________________________________________
            //
            // Handles an assignment from etree_rval to etree_lval.

            def rvalueOrNull(
                etree_lval: ExpressionTree,
                oetree_rval: Option[ExpressionTree]
            ): ir.Path = 
                oetree_rval match {
                    case None => nullStmt(etree_lval)
                    case Some(etree_rval) => rvalue(etree_rval)
                }

            def assign(
                etree_lval: ExpressionTree,
                oetree_rval: Option[ExpressionTree]
            ): ir.Path = at(etree, "assign", dummyLvalue(_)) {
                etree_lval match {
                    case tree: ArrayAccessTree => // p[q] = r
                        val p = rvalue(tree.getExpression)
                        val q = rvalue(tree.getIndex)
                        val r = rvalueOrNull(etree_lval, oetree_rval)
                        callUnit(tree, p, ir.m_arraySet, q, r)

                    case tree: IdentifierTree => 
                        val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                        val q = rvalueOrNull(etree_lval, oetree_rval)
                        elem.getKind match {
                            case EK.PARAMETER | EK.LOCAL_VARIABLE => // x = q (where x is a new version)
                                val sym = symtab(nm(elem)).sym
                                val ver = sym.nextExprVersion(q)
                                symtab += Pair(nm(elem), ver)

                            case EK.FIELD => // [this.]f = q
                                store(tree, ir.p_this, f(elem), q)

                            case _ =>
                                throw new Unhandled(tree)

                        }

                    case tree: MemberSelectTree => // p.f = q
                        val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                        assert(elem.getKind == EK.FIELD)
                        val p = rvalueIfNotStatic(elem, tree.getExpression)
                        val q = rvalueOrNull(etree_lval, oetree_rval)
                        store(tree, p, f(elem), q)

                    case _ =>
                        throw new Unhandled(etree)                    
                }
            }

            // ___ Method Invocations _______________________________________________
            
            def nonIntrinsicMethodInvocation(
                eelem: ExecutableElement,
                mitree: MethodInvocationTree
            ): ir.Path = mitree.getMethodSelect match {
                case tree: IdentifierTree => // [this].m(...) or super.m(...)
                    val m = ttf.m(eelem)
                    tree.getName.toString match {
                        case "super" => 
                            eelem.getKind match {
                                case EK.CONSTRUCTOR => 
                                    val qs = mitree.getArguments.map(rvalue)
                                    mblk_cur.addStmt(
                                        mitree, 
                                        ir.StmtSuperCtor(m, qs.toList))
                                    ir.p_this // Dummy return value.
                                    
                                case _ => 
                                    val qs = mitree.getArguments.map(rvalue)
                                    val r = freshVar
                                    mblk_cur.addStmt(
                                        mitree, 
                                        ir.StmtSuperCall(r, m, qs.toList))
                                    r.path
                            }

                        case _ =>
                            // XXX Nested classes can use a different this.
                            val qs = mitree.getArguments.map(rvalue)
                            call(mitree, ir.p_this, m, qs: _*))
                    }

                case tree: MemberSelectTree => // p.m(...)
                    val p = rvalueIfNotStatic(eelem, tree.getExpression)
                    val qs = mitree.getArguments.map(rvalue)
                    call(mitree, p, m, qs: _*)

                case tree =>
                    throw new Unhandled(tree)     
            }  
            
            def methodInvocation(
                mitree: MethodInvocationTree
            ): ir.Path = {
                val eelem = TU.elementFromUse(mitree)

                // First check for intrinsics:
                if(wke.addHb(eelem)) {
                    val qs = mitree.getArguments.map(rvalue)
                    mblk_cur.addStmt(mitree, ir.StmtHb(qs(0), qs(1)))
                    ir.p_this // Dummy return value.
                } else {
                    nonIntrinsicMethodInvocation(eelem, mitree)
                }            
            }

            // ___ Rvalue: Evaluating an expression to a path _______________________

            def rvalueIfNotStatic(elem: Element, etree: ExpressionTree): ir.Path =
                if(EU.isStatic(elem)) throw new Unhandled(etree) // XXX static
                else rvalue(etree)
                
            def rvalue(
                etree: ExpressionTree
            ): ir.Path = at(etree, "rvalue", nullStmt(scope, etree)) {
                etree match {
                    case tree: ArrayAccessTree => // p[q]                
                        val p = rvalue(tree.getExpression)
                        val q = rvalue(tree.getIndex)
                        call(tree, p, ir.m_arrayGet, q)

                    case tree: AssignmentTree => // lvalue = q
                        assign(tree.getVariable, Some(tree.getExpression))

                    case tree: BinaryTree => // p + q (or some other operator)
                        toString(tree.getLeftOperand)
                        toString(tree.getRightOperand)
                        nullStmt(tree)

                    case tree: CompoundAssignmentTree => // p += q
                        toString(tree.getVariable)
                        toString(tree.getExpression)
                        assign(tree.getVariable, None)

                    case tree: ConditionalExpressionTree => // p = (cond ? q : r)
                        rvalue(tree.getCondition) // we don't really need to use the condition
                        
                        // Result variable:
                        val lv_res = freshVar
                        val wt_res = wtref(etree)
                        
                        subscope(ScopeKindSwitch, None, ir.LvDecl(lv_res, wt_res)) { scope_sw =>
                            scope_sw.subscope(ScopeSeq, None) { scope_t =>
                                val p_t = scope_t.rvalue(tree.getTrueExpression)                        
                                scope_t.uncondBreak(1, p_t)
                            }
                            
                            scope_sw.subscope(ScopeSeq, None) { scope_f =>
                                val p_f = scope_f.rvalue(tree.getFalseExpression)
                                scope_f.uncondBreak(1, p_f)                                
                            }
                        }
                        
                        lv_res.path

                    case tree: IdentifierTree => 
                        val elem = TU.elementFromUse(tree)
                        val nm_elem = nm(elem)
                        if(nm_elem == "this")
                            ir.p_this // TODO: Inner classes also use qualified forms of this
                        else elem.getKind match {
                            case EK.PARAMETER | EK.LOCAL_VARIABLE => symtab(nm_elem).p
                            case EK.FIELD => load(tree, ir.p_this, f(elem))
                            case EK.METHOD => ir.p_this // happens for "implicit this" calls like foo(...)
                            case EK.CONSTRUCTOR => throw new Unhandled(tree)
                            case _ => throw new Unhandled(tree)
                        }

                    case tree: LiteralTree =>
                        nullStmt(tree)

                    case tree: MemberSelectTree =>
                        val elem = TU.elementFromUse(tree)
                        if(elem.getKind.isField) { // p.f
                            val p = rvalue(tree.getExpression)
                            load(tree, p, f(elem))
                        } else { // p.m, just evaluate receiver
                            rvalue(tree.getExpression)
                        }

                    case tree: MethodInvocationTree => // p.m(Q)
                        methodInvocation(tree)

                    case tree: NewClassTree => // p = new T(Q)
                        val lv = freshVar
                        val elem = IU.constructor(tree)
                        val t = chkTref(wtref(tree))
                        val m = ttf.m(elem)
                        val qs = tree.getArguments.map(rvalue).toList
                        addStmt(tree, ir.StmtNew(lv, t, m, qs))
                        lv.path

                    case tree: UnaryTree => // !p, p++, p-- etc
                        assert(getAnnotatedType(tree).getKind.isPrimitive)
                        tree.getKind match {
                            // p++, p-- are effectively writes:
                            case TRK.POSTFIX_DECREMENT | TRK.POSTFIX_INCREMENT | TRK.PREFIX_DECREMENT | TRK.PREFIX_INCREMENT =>
                                assign(tree.getExpression, None)
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
                tree: StatementTree,
            ): Unit = at(tree, "stmt", ()) {
                case tree: AssertTree =>
                    rvalue(tree.getCondition)

                case tree: BlockTree =>
                    subscope(ScopeSeq, label_tree) { scope_blk =>
                        tree.getStatements.foreach(scope_blk.stmt(None, _))
                        scope_blk.uncondBreak(0)                        
                    }

                case tree: BreakTree =>
                    val idx = branchIndex(BranchBreak, nullToOption(tree.getLabel))
                    uncondBreak(idx)

                case tree: ClassTree => 
                    throw new Unhandled(tree) // XXX inner classes

                case tree: BreakTree =>
                    val idx = branchIndex(BranchContinue, nullToOption(tree.getLabel))
                    uncondContinue(idx)

                case tree: DoWhileLoopTree =>
                    // The "outer" scope is the target for breaks, 
                    // "inner" is the target for continues.
                    subscope(ScopeSeq, label_tree) { scope_outer =>
                        subscope(ScopeLoop, label_tree) { scope_inner =>
                            scope_inner.stmt(None, tree.getStatement)
                            scope_inner.rvalue(tree.getCondition)
                            scope_inner.condBreak(1)
                            scope_inner.uncondContinue(0)                            
                        }
                    }

                case tree: EmptyStatementTree => ()

                case tree: EnhancedForLoopTree => throw new Unhandled(tree) // XXX enhanced for loops

                case tree: ExpressionStatementTree => rvalue(tree.getExpression)
                
                case tree: ForLoopTree =>
                    // Rather involved transformation:
                    //  outer: Seq {
                    //      <initializers>
                    //      <condition>
                    //      condBreak Seq;
                    //      <statement>
                    //      inner: Loop {
                    //          <update>
                    //          <condition>
                    //          condBreak Seq;
                    //          <statement>
                    //          continue Loop;
                    //      }
                    //  }
                    
                    subscope(ScopeSeq, label_tree) { scope_outer =>
                        tree.getInitializer.foreach(scope_outer.stmt(None, _))
                        scope_outer.rvalue(tree.getCondition)
                        scope_outer.condBreak(0)
                        scope_outer.stmt(None, tree.getStatement)                        
                        
                        subscope(ScopeLoop, label_tree) { scope_inner =>
                            tree.getUpdate.foreach(scope_inner.stmt(None, _))
                            scope_inner.rvalue(tree.getCondition)
                            scope_inner.condBreak(1)
                            scope_inner.stmt(None, tree.getStatement)
                            scope_inner.uncondContinue(0)
                        }
                    }
                    
                case tree: LabeledStatementTree =>
                    stmt(tree.getStatement, Some(tree.getLabel))

                case tree: VariableTree =>
                    val sym = createSymbol(mblk_cur.env(tree))(tree)
                    val ver = tree.getInitializer match {
                        case null => sym.nextLocalVersion
                        case expr => 
                            val p = rvalue(expr)
                            sym.nextExprVersion(p)
                    }
                    mblk_cur.symtab += Pair(sym.name, ver)

                case tree: WhileLoopTree =>
                    // The "outer" scope is the target for breaks, 
                    // "inner" is the target for continues.
                    subscope(ScopeSeq, label_tree) { scope_outer =>
                        subscope(ScopeLoop, label_tree) { scope_inner =>
                            scope_inner.rvalue(tree.getCondition)
                            scope_inner.condBreak(1)
                            scope_inner.stmt(None, tree.getStatement)
                            scope_inner.uncondContinue(0)                            
                        }
                    }

                case _ => throw new Unhandled(tree)

            }
            
        }
        
        // ___ fork() and goto() ________________________________________________
        // Create and link new internal blocks, carrying along in-scope variables.


        // Tranfers control to the closest scope for which the
        // function returns Some(_), executing any finally scopes
        // we find along the way.  [Note: transferFunc() must return
        // None for any Finally scope.]  The current block upon return
        // will execute if no corresponding scope is found (often impossible).
        def transfer(treePos: Tree)(transferFunc: (Scope => Option[MutableBlock])) {
            def passThru(ss: List[Scope]): Unit = ss match {
                case Scope(Finally(Some(blockTree)), _) :: tl =>
                    withScopes(tl) {
                        stmt(blockTree)
                    }
                    passThru(tl)

                case hd :: tl =>
                    transferFunc(hd) match {
                        case Some(mblk_tar) =>
                            goto(treePos, mblk_tar)
                            start(fork(treePos))
                        case None =>
                            passThru(tl)
                    }

                case List() =>
            }

            passThru(scopes)
        }

    // ___ Building the MutableBlocks array 'mblks' _________________________

    stmt(mtree.getBody)

    // ___ Converting to an Array[ir.Block] _________________________________

    mblks.toArray[MutableBlock].map { mblk =>
        ir.Block(
            mblk.paramDecls,
            mblk.stmts.toList,
            mblk.gotos.toList
        )
    }

}