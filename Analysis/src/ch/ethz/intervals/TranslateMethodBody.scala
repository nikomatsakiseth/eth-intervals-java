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

        // ___ Mutable blocks ___________________________________________________
        // We generate blocks into these mutable structures.

        class MutableBlock(
            val b: Int,
            val autoParamNames: List[String],
            val paramDecls: List[ir.LvDecl],
            var symtab: SymbolTable,
            val pos: DummyPosition
        ) {
            val stmts = new ListBuffer[ir.Stmt]()
            val gotos = new ListBuffer[ir.Goto]()
            
            def env(tree: Tree): ttf.TranslateEnv = {
                val m_lvs = symtab.foldLeft(env_mthd.m_lvs) { case (m, (name, ver)) =>
                    m + Pair(name, (ver.p, ver.sym.annty))
                }
                new ttf.TranslateEnv(TreePosition(tree), m_lvs, env_mthd.m_defaultWghosts)
            }

            def addStmt(treePos: Tree, stmt: ir.Stmt) = 
                stmts += withPos(TreePosition(treePos), stmt)
        } 

        // ___ Current block ____________________________________________________
        // mblks and mblk are updated as we generate code
        val mblks = new ArrayBuffer[MutableBlock]()
        mblks += new MutableBlock(0, List(), List(), symtab_mthdParam, TreePosition(mtree))
        var mblk_cur = mblks(0)

        // ___ fork() and goto() ________________________________________________
        // Create and link new internal blocks, carrying along in-scope variables.

        def fork(treePos: Tree, xtra: (ir.WcTypeRef, ir.VarName)*): MutableBlock = {
            var symtab = symtab_mthdParam // always start with method parameters as the basis

            val autoParamNames = new ListBuffer[String]()
            val paramDecls = new ListBuffer[ir.LvDecl]()
            for((name, ver0) <- mblk_cur.symtab if ver0.isLocal) {
                val ver1 = ver0.sym.nextLocalVersion
                autoParamNames += name
                paramDecls += ver1.toLvDecl
                symtab += Pair(name, ver1)
            }

            for((wt, lv) <- xtra)
                paramDecls += ir.LvDecl(lv, wt)

            mblks += new MutableBlock(
                mblks.length, 
                autoParamNames.toList, 
                paramDecls.toList, 
                symtab, 
                TreePosition(treePos))
            mblks.last
        }

        // Switch current block to 'mblk_new', returning 'mblk_new'
        def start(mblk_new: MutableBlock) = {
            mblk_cur = mblk_new
            mblk_new // just convenient
        }

        // Creates a branch to b, providing current values for any in-scope
        // variables and also the additional values 'ps_xtra'.  Returns 'mblk_tar'.
        def goto(treePos: Tree, mblk_tar: MutableBlock, ps_xtra: ir.Path*) = {
            val ps_auto = mblk_tar.autoParamNames.map(nm => mblk_cur.symtab(nm).p)
            mblk_cur.gotos += withPos(TreePosition(treePos), ir.Goto(mblk_tar.b, ps_auto ++ ps_xtra))
            mblk_tar // just convenient
        }

        // ___ Scopes ___________________________________________________________
        // Used to handle tricky, block-structured control-flow like try/finally

        sealed abstract class ScopeKind
        sealed case class Finally(block: Option[BlockTree]) extends ScopeKind
        sealed abstract class TargetScopeKind extends ScopeKind {
            def mblk_tar: MutableBlock
        }
        sealed case class Continue(mblk_tar: MutableBlock) extends TargetScopeKind
        sealed abstract class Break extends TargetScopeKind
        sealed case class AnonBreak(mblk_tar: MutableBlock) extends Break
        sealed case class BlockBreak(mblk_tar: MutableBlock) extends Break
        case class Catch(mblk_tar: MutableBlock) extends TargetScopeKind

        case class Scope(scopeKind: ScopeKind, label: Option[Name]) 

        val mblk_uncaught = fork(mtree)
        var scopes: List[Scope] = List(Scope(Catch(mblk_uncaught), None)) // always a catch block in scope
        val treeLabels = MutableMap[Tree, Name]() // track labels assigned to different tree nodes

        def withScopes(newScopes: List[Scope])(func: => Unit) {
            val oldScopes = scopes
            try {
                scopes = newScopes
                func                                    
            } finally {
                scopes = oldScopes                
            }            
        }

        // Execute func with Scope(kind) in-scope, then restore scopes
        def inScope(kind: ScopeKind, tree: Tree)(func: => Unit) = 
            withScopes(Scope(kind, treeLabels.get(tree)) :: scopes) {
                log.indented("inScope(%s, %s)", kind, tree) { func }
            }

        // ___ Statement Constructors ___________________________________________

        def call(treePos: Tree, p: ir.Path, m: ir.MethodName, qs: ir.Path*) = {
            val r = freshVar
            mblk_cur.addStmt(treePos, ir.StmtCall(r, p, m, qs.toList))
            r.path
        }

        def callUnit(treePos: Tree, p: ir.Path, m: ir.MethodName, qs: ir.Path*) {
            call(treePos, p, m, qs: _*)
        }

        def load(treePos: Tree, p: ir.Path, f: ir.FieldName) = {
            val r = freshVar
            mblk_cur.addStmt(treePos, ir.StmtGetField(r, p, f))
            r.path
        }

        def store(treePos: Tree, p: ir.Path, f: ir.FieldName, q: ir.Path) = {
            mblk_cur.addStmt(treePos, ir.StmtSetField(p, f, q))
        }

        // ___ Translating ExpressionTrees ______________________________________

        def dummyLvalue(p: ir.Path) { }

        // ______ Lvalues _______________________________________________________
        // Processes an expression as an lvalue.  Returns a function which, when 
        // applied to the variable generated by the right-hand side, creates the 
        // IR to store it.  
        // The technique of returning a function allows us to ensure that
        // the generated IR reflects Java's left-to-right evaluation order.
        def lvalue(etree: ExpressionTree): (ir.Path => Unit) = at(etree, "lvalue", dummyLvalue(_)) {
            etree match {
                case tree: ArrayAccessTree => // p[q] = r
                    val p = rvalue(tree.getExpression)
                    val q = rvalue(tree.getIndex)
                    (r => callUnit(tree, p, ir.m_arraySet, q, r))

                case tree: IdentifierTree => 
                    val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                    elem.getKind match {
                        case EK.PARAMETER | EK.LOCAL_VARIABLE => // x = q (where x is a new version)
                            { q =>
                                val sym = mblk_cur.symtab(nm(elem)).sym
                                val ver = sym.nextExprVersion(q)
                                mblk_cur.symtab += Pair(nm(elem), ver)
                            }

                        case EK.FIELD => // [this.]f = q
                            { q => store(tree, ir.p_this, f(elem), q) }

                        case _ =>
                            throw new Unhandled(tree)

                    }

                case tree: MemberSelectTree => // p.f = q
                    val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                    assert(elem.getKind == EK.FIELD)
                    val p = rvalueIfNotStatic(elem, tree.getExpression)
                    (q => store(tree, p, f(elem), q))

                case _ =>
                    throw new Unhandled(etree)                    
            }
        }

        // ______ Method Selectors ______________________________________________
        // Returns a function which, when given the list of arguments, generates
        // appropriate statements to invoke the method selected by 'etree'.
        // The technique of returning a function allows us to ensure that
        // the generated IR reflects Java's left-to-right evaluation order.
        def methodSelect(mitree: MethodInvocationTree): (Seq[ir.Path] => ir.Path) = {
            val eelem = TU.elementFromUse(mitree)
            val m = ttf.m(eelem)

            if(wke.addHb(eelem)) {
                { qs => // Add edge p -> q
                    mblk_cur.addStmt(mitree, ir.StmtHb(qs(0), qs(1)))
                    ir.p_this // Dummy return value.
                }
            } else {
                mitree.getMethodSelect match {
                    case tree: IdentifierTree => // [this].m(...) or super.m(...)
                        tree.getName.toString match {
                            case "super" => 
                                eelem.getKind match {
                                    case EK.CONSTRUCTOR => 
                                        // Super ctor calls have no return value, just use 'this'
                                        { qs =>
                                            mblk_cur.addStmt(mitree, ir.StmtSuperCtor(m, qs.toList))
                                            ir.p_this // Dummy return value.
                                        }
                                    case _ => 
                                        { qs =>
                                            val r = freshVar
                                            mblk_cur.addStmt(mitree, ir.StmtSuperCall(r, m, qs.toList))
                                            r.path
                                        }
                                }

                            case _ =>
                                // XXX Nested classes can use a different this.
                                (qs => call(mitree, ir.p_this, m, qs: _*))
                        }

                    case tree: MemberSelectTree => // p.m(...)
                        val p = rvalueIfNotStatic(eelem, tree.getExpression)
                        (qs => call(mitree, p, m, qs: _*))

                    case tree =>
                        throw new Unhandled(tree)     
                }                
            }            
        }
        
        // ______ Rvalues _______________________________________________________
        def toString(etree: ExpressionTree) {
            val p = rvalue(etree)
            val annty = getAnnotatedType(etree)
            if(!annty.getKind.isPrimitive)
                callUnit(etree, p, ir.m_toString)
        }

        def wtref(etree: ExpressionTree) = {
            ttf.wtref(mblk_cur.env(etree))(getAnnotatedType(etree))
        }
        
        def rvalueIfNotStatic(elem: Element, etree: ExpressionTree): ir.Path =
            if(EU.isStatic(elem)) throw new Unhandled(etree) // XXX static
            else rvalue(etree)

        // Generates a path with the same type as etree but a null value.
        // Used to model scalar operations, like x + y, where the actual result 
        // doesn't matter to us.
        def nullStmt(etree: ExpressionTree) = {
            val r = freshVar
            mblk_cur.addStmt(etree, ir.StmtNull(r, wtref(etree)))
            r.path
        }

        def rvalue(etree: ExpressionTree): ir.Path = at(etree, "result", nullStmt(etree)) {
            etree match {
                case tree: ArrayAccessTree => // p[q]
                    val p = rvalue(tree.getExpression)
                    val q = rvalue(tree.getIndex)
                    call(tree, p, ir.m_arrayGet, q)

                case tree: AssignmentTree => // lvalue = q
                    val addr = lvalue(tree.getVariable)
                    val q = rvalue(tree.getExpression)
                    addr(q)      
                    
                    // Strictly speaking, the result ought to be
                    // rvalue(tree.getVariable), but that generates
                    // annoying double errors if the assignment
                    // is illegal.  The only problem with using q is that its
                    // type may be a subtype of the LHS.  Oh well.
                    // We COULD insert an upcast, but it hardly seems worth it.
                    q 

                case tree: BinaryTree => // p + q (or some other operator)
                    toString(tree.getLeftOperand)
                    toString(tree.getRightOperand)
                    nullStmt(tree)

                case tree: CompoundAssignmentTree => // p += q
                    toString(tree.getVariable)
                    toString(tree.getExpression)
                    val addr = lvalue(tree.getVariable)
                    val r = nullStmt(tree)
                    addr(r)
                    rvalue(tree.getVariable) // Result of an assignment is the LHS.

                case tree: ConditionalExpressionTree => // p = (cond ? q : r)
                    rvalue(tree.getCondition) // we don't really need to use the condition
                    val mblk_t = goto(tree, fork(tree))
                    val mblk_f = goto(tree, fork(tree))

                    // Join block will need an extra param for our result:
                    val lv = freshVar
                    val wt = wtref(etree)
                    val mblk_j = fork(tree, (wt, lv))

                    start(mblk_t)
                    val q = rvalue(tree.getTrueExpression)
                    goto(tree, mblk_j, q)

                    start(mblk_f)
                    val r = rvalue(tree.getFalseExpression)
                    goto(tree, mblk_j, r)

                    start(mblk_j)
                    lv.path

                case tree: IdentifierTree => 
                    val elem = TU.elementFromUse(tree)
                    val nm_elem = nm(elem)
                    if(nm_elem == "this")
                        ir.p_this // TODO: Inner classes also use qualified forms of this
                    else elem.getKind match {
                        case EK.PARAMETER | EK.LOCAL_VARIABLE => mblk_cur.symtab(nm_elem).p
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
                    val mfunc = methodSelect(tree)
                    val qs = tree.getArguments.map(rvalue)
                    mfunc(qs)

                case tree: NewClassTree => // p = new T(Q)
                    val lv = freshVar
                    val elem = IU.constructor(tree)
                    val t = chkTref(wtref(tree))
                    val m = ttf.m(elem)
                    val qs = tree.getArguments.map(rvalue).toList
                    mblk_cur.addStmt(tree, ir.StmtNew(lv, t, m, qs))
                    lv.path

                case tree: UnaryTree => // !p, p++, p-- etc
                    assert(getAnnotatedType(tree).getKind.isPrimitive)
                    val p = nullStmt(tree)
                    tree.getKind match {
                        // p++, p-- are effectively writes:
                        case TRK.POSTFIX_DECREMENT | TRK.POSTFIX_INCREMENT | TRK.PREFIX_DECREMENT | TRK.PREFIX_INCREMENT =>
                            val addr = lvalue(tree.getExpression)
                            addr(p)
                        case _ => ()
                    }
                    p

                case _ =>
                    throw new Unhandled(etree)
            } 
        }

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

        // Transfer control to the nearest in-scope catch
        def mustThrow(tree: Tree) {
            transfer(tree) {
                case Scope(Catch(target), _) => Some(target)
                case _ => None
            }    
        }

        def mayThrow(tree: Tree) {
            val mblk_throw = goto(tree, fork(tree))
            val mblk_noThrow = goto(tree, fork(tree))

            start(mblk_throw)
            mustThrow(tree)

            start(mblk_noThrow)
        }

        // Generates statements equivalent to 'tree'
        def stmt(tree: StatementTree): Unit = tree match {
            case tree: AssertTree =>
                rvalue(tree.getCondition)

            case tree: BlockTree =>
                val mblk_break = fork(tree)
                inScope(BlockBreak(mblk_break), tree) {
                    tree.getStatements.foreach(stmt)
                    goto(tree, mblk_break)
                }
                start(mblk_break)

            case tree: BreakTree =>
                nullToOption(tree.getLabel) match {
                    case None => // "break;" transfers to the nearest AnonBreak w/ any label:
                        transfer(tree) {
                            case Scope(b: AnonBreak, _) => Some(b.mblk_tar)
                            case _ => None                        
                        }
                    case Some(l) => // "break l;" transfers to the nearest break w/ label l:
                        transfer(tree) {
                            case Scope(b: Break, Some(l1)) if (l == l1) => Some(b.mblk_tar)
                            case _ => None
                        }
                }

            case tree: ClassTree => throw new Unhandled(tree) // XXX inner classes

            case tree: ContinueTree =>
                nullToOption(tree.getLabel) match {
                    case None => // "continue;" transfers to the nearest Continue w/ any label:
                        transfer(tree) {
                            case Scope(Continue(mblk_tar), _) => Some(mblk_tar)
                            case _ => None            
                        }

                    case Some(l) => // "continue l;" transfers to the nearest Continue w/ label l:
                        transfer(tree) {
                            case Scope(Continue(mblk_tar), Some(l1)) if (l == l1) => Some(mblk_tar)
                            case _ => None
                        }
                }

            case tree: DoWhileLoopTree =>
                val mblk_break = fork(tree)
                val mblk_body = start(goto(tree, fork(tree)))

                inScope(AnonBreak(mblk_break), tree) {
                    inScope(Continue(mblk_body), tree) {
                        stmt(tree.getStatement)
                        rvalue(tree.getCondition)
                        goto(tree, mblk_body)
                    }
                }

                start(mblk_break)

            case tree: EmptyStatementTree => ()

            case tree: EnhancedForLoopTree => throw new Unhandled(tree) // XXX enhanced for loops

            case tree: ExpressionStatementTree => rvalue(tree.getExpression)
            
            case tree: VariableTree =>
                val sym = createSymbol(mblk_cur.env(tree))(tree)
                val ver = tree.getInitializer match {
                    case null => sym.nextLocalVersion
                    case expr => 
                        val p = rvalue(expr)
                        sym.nextExprVersion(p)
                }
                mblk_cur.symtab += Pair(sym.name, ver)
            
            case _ => throw new Unhandled(tree)

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
}