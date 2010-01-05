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
import checkers.util.AnnotationUtils.AnnotationBuilder
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

class TranslateMethodBody(ttf: TranslateTypeFactory, mtree: MethodTree) {
    import ttf.TreePosition
    import ttf.ElementPosition
    import ttf.DummyPosition
    import ttf.getAnnotatedType
    import ttf.f
    
    // ___ Miscellany _______________________________________________________
    
    class Unhandled(val tree: Tree) 
    extends RuntimeException("Could not handle: "+tree)
    
    def setPos[X <: Positional](pos: DummyPosition)(v: X) = {
        v.setPos(pos)
        v
    }
    
    var unique = 0
    def freshName(prefix: String) = {
        unique += 1
        prefix + "[" + unique + "]"
    }
    def freshVar() = ir.VarName(freshName("jv"))
    def nm(elem: Element) = elem.getSimpleName.toString
    
    // ___ Symbols and Versions _____________________________________________
    
    // One Symbol per Java variable.
    sealed class Symbol(val name: String, val wtref: ir.WcTypeRef) {
        var maxVersion = 0
        def nextVersion = {
            maxVersion += 1
            Version(this, maxVersion)
        }
    }
    
    // One Version per block where a symbol is used.
    sealed case class Version(sym: Symbol, ver: Int) {
        override def toString = "Version(%s)".format(lv)
        lazy val lv = ir.VarName(sym.name + "[" + ver + "]") 
        lazy val p = lv.path
        def toLvDecl = ir.LvDecl(lv, sym.wtref)
    }
    
    type SymbolTable = Map[String, Version] // ListMap: preserves ordering!

    def createSymbol(env: ttf.TranslateEnv)(vtree: VariableTree): Symbol = {
        val elem = elementFromDeclaration(vtree)
        val annty = ttf.getAnnotatedType(elem)
        val wtref = ttf.wtref(env)(annty)
        new Symbol(nm(elem), wtref)
    }
    
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
        val succs = new ListBuffer[ir.Succ]()
        
        def addStmt(pos: DummyPosition, stmt: ir.Stmt) = stmts += setPos(pos, stmt)
    } 
    
    // ___ Entrypoint _______________________________________________________
    
    def translate = log.indentedRes("translateMethodBody(%s)", mtree) {            
        
        // ___ Method environment, parameters ___________________________________
        val elem_mthd = TU.elementFromDeclaration(mtree)
        val elem_cls = EU.enclosingClass(elem_mthd)
        val env_mthd = ttf.envElem(mthdElem)
        val elems_param = mtree.getParameters.map(TU.elementFromDeclaration).toList
        val syms_param = mtree.getParameters.map(createSymbol(env_mthd)).toList
        val vers_param = syms_param.map(_.nextVersion)
        def isParamVer(v: Version) = vers_param.contains(v)
        val symtab_param: SymbolTable = Map(elems_param.map(nm).zip(vers_param): _*)
        
        // ___ Current block ____________________________________________________
        // blkMap and blk_cur are updated as generate code
        val blks = new ArrayBuffer()
        blks += new MutableBlock(0, List(), List(), symtab_param, TreePosition(mtree))
        var blk = initialBlock
        
        // ___ fork() and goto() ________________________________________________
        // Create and link new internal blocks, carrying along in-scope variables.
            
        def fork(pos: DummyPosition, xtra: (ir.WcTypeRef, ir.VarName)*): MutableBlock = {
            var symtab = symtab_param // always start with method parameters as the basis
            
            val autoParamNames = new ListBuffer[String]()
            val paramDecls = new ListBuffer[ir.LvDecl]()
            for((name, ver0) <- blk.symtab if !isParamVer(ver0)) {
                val ver1 = ver0.sym.nextVersion
                autoParamNames += name
                paramDecls += ver1.toLvDecl
                symtab += Pair(name, ver1)
            }
            
            for((wt, lv) <- xtra)
                paramDecls += ir.LvDecl(lv, wt)
                
            blks += new MutableBlock(blks.length, autoParamNames.toList, paramDecls.toList, symtab, pos)
            blks.last
        }
        
        // Switch current block to 'blk_new', returning 'blk_new'
        def start(blk_new: MutableBlock) = {
            blk = blk_new
            blk_new // just convenient
        }

        // Creates a branch to b, providing current values for any in-scope
        // variables and also the additional values 'ps_xtra'.  Returns 'blk_tar'.
        def goto(pos: DummyPosition, blk_tar: MutableBlock, ps_xtra: ir.Path*) = {
            val ps_auto = blk_tar.paramUserNames.map(nm => blk.symtab(nm).p)
            blk.succs += setPos(pos)(ir.Goto(blk_tar.b, ps_auto ++ ps_xtra))
            blk_tar // just convenient
        }
        
        // ___ Scopes ___________________________________________________________
        // Used to handle tricky, block-structured control-flow like try/finally
        
        sealed abstract class ScopeKind
        sealed case class Finally(block: Option[BlockTree]) extends ScopeKind
        sealed abstract class TargetScopeKind extends ScopeKind {
            def blk_tar: MutableBlock
        }
        sealed case class Continue(blk_tar: MutableBlock)
        sealed abstract class BreakScopeKind extends TargetScopeKind
        sealed case class AnonBreak(blk_tar: MutableBlock) extends BreakScopeKind
        sealed case class BlockBreak(blk_tar: MutableBlock) extends BreakScopeKind
        case class Catch(blk_tar: MutableBlock) extends TargetScopeKind
        
        case class Scope(scopeKind: ScopeKind, label: Option[Name]) 
        
        val blk_uncaught = fork(TreePosition(mtree))
        var scopes: List[Scope] = List(Scope(Catch(blk_uncaught), None)) // always a catch block in scope
        val treeLabels = MutableMap[Tree, Name]() // track labels assigned to different tree nodes
        
        // Execute func with Scope(kind) in-scope, then restore scopes
        def inScope(kind: ScopeKind, tree: Tree)(func: => Unit) {
            val oldScopes = scopes
            try {
                log.indented("inScope(%s, %s)", kind, tree) {
                    scopes = Scope(kind, treeLabels.get(tree)) :: scopes
                    func                                    
                }
            } finally {
                scopes = oldScopes                
            }
        }
        
        // ___ Statement Constructors ___________________________________________
        
        def call(pos: DummyPosition, p: ir.Path, m: ir.MethodName, qs: ir.Path*) = {
            val r = freshVar
            blk.addStmt(pos, ir.StmtCall(r, p, m, qs))
            r
        }
        
        def callUnit(pos: DummyPosition, p: ir.Path, m: ir.MethodName, qs: ir.Path*) {
            call(pos, p, m, qs)
        }
        
        def store(pos: DummyPosition, p: ir.Path, f: ir.Field, q: ir.Path) = {
            blk.addStmt(pos, ir.StmtSetField(p, f, q))
        }
        
        // ___ Translating ExpressionTrees ______________________________________
        
        // Processes an expression as an lvalue.  Returns a function which, when applied to the 
        // variable generated by the right-hand side, creates the IR to store it.
        def lvalue(etree: ExpressionTree): (ir.Path => Unit) = log.indented("lvalue(%s)", etree) {
            etree match {
                case tree: ArrayAccessTree => // p[q] = r
                    val p = rvalue(tree.getExpression)
                    val q = rvalue(tree.getIndex)
                    { r => callUnit(TreePosition(tree), p, ir.m_arraySet, q, r) }
                    
                case tree: IdentifierTree => 
                    val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                    elem.getKind match {
                        case EK.PARAMETER | EK.LOCAL_VARIABLE => // x = q (where x is a new version)
                            { q =>
                                val sym = blk.symtab(nm(elem)).sym
                                val ver = sym.nextVersion
                                blk.symtab += Pair(nm(elem), ver)
                                blk.addStmt(TreePosition(tree), ir.StmtAssign(ver.lv, q))
                            }
                            
                        case EK.FIELD => // [this.]f = q
                            { q => store(ir.p_this, f(elem), q) }
                            
                        case _ =>
                            throw new Unhandled(tree)
                        
                    }
                    
                case tree: MemberSelectTree => // p.f = q
                    val elem = TU.elementFromUse(tree).asInstanceOf[VariableElement]
                    assert(elem.getKind == EK.FIELD)
                    val p = rvalueIfNotStatic(elem, tree.getExpression)
                    { q => store(p, f(elem), q) }
            }
        }
    }
    
    def toString(etree: ExpressionTree) {
        val p = rvalue(etree)
        val annty = tctx.getAnnotatedType(atree)
        if(!annty.getKind.isPrimitive)
            callUnit(TreePosition(etree), p, ir.m_toString)
    }
    
    def wtref(etree: ExpressionTree) = {
        ttf.wtref(blk.env)(getAnnotatedType(tree))
    }
    
    def rvalueIfNotStatic(elem: Element, etree: ExpressionTree): ir.Path = {
        if(isStatic(elem)) throw new Unhandled(etree) // XXX static
        else toPath(etree)
    }
    
    // Generates a path with the same type as etree but a null value.
    // Used to model scalar operations, like x + y, where the actual result 
    // doesn't matter to us.
    def nullStmt(etree: ExpressionTree) = {
        val r = freshVar.path
        blk.addStmt(TreePosition(tree), ir.StmtNull(r, wtref(etree)))
        r        
    }
            
    def rvalue(etree: ExpressionTree): ir.Path = log.indentedRes("rvalue(%s)", etree) {
        etree match {
            case tree: ArrayAccessTree => // p[q]
                val p = rvalue(tree.getExpression)
                val q = rvalue(tree.getIndex)
                call(TreePosition(tree), p, i.m_arrayGet, q)
                
            case tree: AssignmentTree => // lvalue = q
                val addr = lvalue(tree.getVariable)
                val q = rvalue(tree.getExpression)
                addr(q)
                rvalue(tree.getVariable) // I don't think this is 100% correct...but it'll do.
                
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
                r
                
            case tree: ConditionalExpressionTree => // p = (cond ? q : r)
                rvalue(tree.getCondition) // we don't really need to use the condition
                val blk_t = goto(tree, fork(tree))
                val blk_f = goto(tree, fork(tree))
                
                // Join block will need an extra param for our result:
                val lv = freshVar
                val wt = wtref(etree)
                val blk_j = fork(tree, (wt, lv))
                
                start(blk_t)
                val q = rvalue(tree.getTrueExpression)
                goto(tree, blk_j, q)

                start(blk_f)
                val r = rvalue(tree.getFalseExpression)
                goto(tree, blk_j, r)
                
                start(blk_j)
                lv.path
                
            case tree: IdentifierTree => 
                val nm_elem = nm(elem)
                val elem = TU.elementFromUse(tree)
                if(nm_elem == "this")
                    ir.p_this // TODO: Inner classes also use qualified forms of this
                else elem.getKind match {
                    case EK.PARAMETER | EK.LOCAL_VARIABLE => blk.symtab(nm_elem).p
                    case EK.FIELD => loadField(TreePosition(tree), ir.p_this, f(elem))
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
                    loadField(TreePosition(tree), p, f(elem))
                } else { // p.m, just evaluate receiver
                    rvalue(tree.getExpression)
                }
                
            case tree: MethodInvocationTree => // p.m(Q)
                val elem = TU.elementFromUse(tree)
                val p = rvalueIfNotStatic(elem, tree.getMethodSelect)
                val m = ttf.m(elem)
                val qs = tree.getArguments.map(rvalue).toList
                call(TreePosition(tree), p, m, qs)
                
            case tree: NewClassTree => // p = new T(Q)
                val lv = freshVar
                val elem = IU.constructor(tree)
                val t = chkTref(wtref(tree))
                val m = ttf.m(elem)
                val qs = tree.getArguments.map(rvalue).toList
                blk.addStmt(TreePosition(tree), ir.StmtNew(lv, t, m, qs))
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
        }
    }

}