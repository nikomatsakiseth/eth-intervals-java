package ch.ethz.intervals

import checkers.source.SourceChecker
import checkers.source.SourceVisitor
import checkers.types.AnnotatedTypeFactory
import checkers.types.AnnotatedTypeMirror

import checkers.util.{ElementUtils => EU}
import checkers.util.{TreeUtils => TU}
import checkers.util.{AnnotationUtils => AU}
import checkers.util.{InternalUtils => IU}

import javax.annotation.processing.ProcessingEnvironment
import javax.lang.model.element._
import javax.lang.model.element.{ElementKind => EK}
import javax.lang.model.`type`._
import javax.lang.model.`type`.{TypeKind => TK}
import javax.lang.model.util.{ElementFilter => EF}
import javax.lang.model.util.ElementScanner6

import com.sun.source.tree._
import com.sun.source.util.TreePath
import com.sun.source.util.TreeScanner

import scala.collection.immutable.Set
import scala.collection.mutable.{Set => MutableSet}
import scala.collection.jcl.Conversions._

import java.io.File

import ch.ethz.intervals.log.LogStack
import ch.ethz.intervals.log.LogDirectory
import ch.ethz.intervals.log.Log
import ch.ethz.intervals.log.SplitLog

class IntervalsChecker extends SourceChecker {

    var logStack: LogStack = null
    def indexLog = logStack.indexLog
    def log = logStack.log

    override def init(env: ProcessingEnvironment) = {
        super.init(env)
        
        logStack = new LogStack(
            env.getOptions.get("INTERVALS_DEBUG_DIR") match {
                case null => 
                    SplitLog.devNullSplitLog
                case dir =>
                    val logDirectory = new LogDirectory(new File(dir))
                    logDirectory.mainSplitLog
            }
        )
    }
        
    override def createFactory(root: CompilationUnitTree): TranslateTypeFactory =
        new TranslateTypeFactory(logStack, this, root)
        
    override def createSourceVisitor(root: CompilationUnitTree) =
        null
    
    override def typeProcess(
        telem: TypeElement,
        treePath: TreePath
    ) = indexLog.indented("IntervalsChecker.typeProcess(%s,%s)".format(telem, treePath)) {
        telem.getKind match {
            case EK.CLASS | EK.INTERFACE | EK.ENUM => {
                currentRoot = treePath.getCompilationUnit
                currentPath = treePath        
                val ttf = createFactory(currentRoot)
                val tctx = new TranslateContext(logStack, ttf)
                
                val classTree = treePath.getLeaf.asInstanceOf[ClassTree]
                val refdElems = referenceClosure(classTree)
                
                indexLog.indented("Build IR") {
                    tctx.addClassImplementation(classTree) // XXX Supertypes?
        			refdElems -= TU.elementFromDeclaration(classTree)                    
        			
                    val typeElemKinds = Set(EK.CLASS, EK.INTERFACE, EK.ENUM, EK.ANNOTATION_TYPE)
                    refdElems.filter(e => typeElemKinds(e.getKind)).foreach(refdElem =>
                        tctx.addClassInterface(refdElem.asInstanceOf[TypeElement], refdElems))
                }
                
                indexLog.indented("Constructed IR") {
                    tctx.cds.foreach { cd =>
                        indexLog.indented("Class %s", cd.name) {
                            log.classDecl("Generated: ", cd)
                        }
                    }
                }
                
            }
            case _ => ()
        }
    }
    
	def addType(referencedElements: MutableSet[Element], tm: TypeMirror): Unit =
	    tm.getKind match {
	        case TK.ARRAY =>
	            addType(referencedElements, tm.asInstanceOf[ArrayType].getComponentType)
	        case TK.DECLARED =>
	            referencedElements += tm.asInstanceOf[DeclaredType].asElement
	        case TK.TYPEVAR =>
	            referencedElements += tm.asInstanceOf[TypeVariable].asElement
	        case _ =>
	            ()
	    }
	    
    class TreeVisitor extends TreeScanner[Void, Void] {
        val referencedElements = MutableSet.empty[Element]
        
        def scanBodyOf(tree: ClassTree) {
            referencedElements += TU.elementFromDeclaration(tree)
            super.visitClass(tree, null)            
        }
        
        override def visitClass(tree: ClassTree, v: Void): Void = {
            null // don't visit nested classes
        }
        
        override def visitMemberSelect(tree: MemberSelectTree, v: Void): Void = {
            referencedElements += TU.elementFromUse(tree)
            super.visitMemberSelect(tree, v)
        }
        
        override def visitMethodInvocation(tree: MethodInvocationTree, v: Void): Void = {
            referencedElements += TU.elementFromUse(tree)
            super.visitMethodInvocation(tree, v)
        }
        
        override def visitNewClass(tree: NewClassTree, v: Void): Void = {
			referencedElements += IU.constructor(tree)
			super.visitNewClass(tree, v)
        }
        
        override def visitMethod(tree: MethodTree, v: Void): Void = {
            val elem = TU.elementFromDeclaration(tree)
            addType(referencedElements, elem.asType)
            super.visitMethod(tree, v)
        }
        
        override def visitVariable(tree: VariableTree, v: Void): Void = {
            val elem = TU.elementFromDeclaration(tree)
            addType(referencedElements, elem.asType)
            super.visitVariable(tree, v)
        }
    }
    
    class ElementVisitor extends ElementScanner6[Void, Void] {
        val referencedElements = MutableSet.empty[Element]

        override def visitExecutable(elem: ExecutableElement, v: Void): Void = {
            referencedElements += EU.enclosingClass(elem)
            addType(referencedElements, elem.getReturnType)
            super.scan(elem.getParameters, v)
        }
        
        override def visitType(elem: TypeElement, v: Void): Void = {
            elem.getInterfaces.foreach(addType(referencedElements, _))
            addType(referencedElements, elem.getSuperclass)
            null // don't visit enclosed elements
        }
        
        override def visitTypeParameter(elem: TypeParameterElement, v: Void): Void = {
            elem.getBounds.foreach(addType(referencedElements, _))
            null
        }
        
        override def visitVariable(elem: VariableElement, v: Void): Void = {
            referencedElements += EU.enclosingClass(elem)
            addType(referencedElements, elem.asType)
            null
        }
    }
    
    def referenceClosure(tree: ClassTree): MutableSet[Element] = {
        def addElements(
            resultSet: MutableSet[Element],
            newSet: MutableSet[Element]
        ): MutableSet[Element] = {
            val oldSize = resultSet.size
            resultSet ++= newSet
            if(resultSet.size == oldSize) {
                resultSet
            } else {
                val ev = new ElementVisitor()
                newSet.foreach(ev.scan(_, null))
                addElements(resultSet, ev.referencedElements)
            }
        }
        
        val tv = new TreeVisitor()
        val resultSet = MutableSet.empty[Element]
        tv.scanBodyOf(tree)
        addElements(resultSet, tv.referencedElements)
    }
    
}