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
import scala.collection.JavaConversions._

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
                val refdElems = referenceClosure(ttf, classTree)
                
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
                
                indexLog.indented("Run type checker") {
                    val prog = tctx.createProg
                    new CheckAll(prog).check                  
                }
                
                indexLog.indented("Reporting Errors") {
                    ttf.reportErrorsFromLogStack()
                }
                
            }
            case _ => ()
        }
    }
    
	def addType(referencedElements: MutableSet[Element], tm: TypeMirror): Unit = tm.getKind match {
        case TK.ARRAY =>
            addType(referencedElements, tm.asInstanceOf[ArrayType].getComponentType)
        case TK.DECLARED =>
            referencedElements += tm.asInstanceOf[DeclaredType].asElement
        case TK.TYPEVAR =>
            referencedElements += tm.asInstanceOf[TypeVariable].asElement
        case _ =>
            ()
    }	    
	    
	def addTypesOfAnnotations(referencedElements: MutableSet[Element], elem: Element): Unit = {
	    elem.getAnnotationMirrors.foreach { am =>
	        addType(referencedElements, am.getAnnotationType)
	    }
	}
	
    class TreeExplorer(ttf: TranslateTypeFactory) extends TreeScanner[Unit, Boolean] {
        val referencedElements = MutableSet.empty[Element]
        
        override def visitClass(tree: ClassTree, r: Boolean) {
            if(r) {
                referencedElements += TU.elementFromDeclaration(tree)
                super.visitClass(tree, false)                            
            }
        }
        
        override def visitMemberSelect(tree: MemberSelectTree, r: Boolean) {
            referencedElements += TU.elementFromUse(tree)
            super.visitMemberSelect(tree, r)
        }
        
        override def visitMethodInvocation(tree: MethodInvocationTree, r: Boolean) {
            referencedElements += TU.elementFromUse(tree)
            super.visitMethodInvocation(tree, r)
        }
        
        override def visitNewClass(tree: NewClassTree, r: Boolean) {
			referencedElements += IU.constructor(tree)
			super.visitNewClass(tree, r)
        }
        
        override def visitMethod(tree: MethodTree, r: Boolean) {
            val elem = TU.elementFromDeclaration(tree)
            addType(referencedElements, elem.asType)
            super.visitMethod(tree, r)
        }
        
        override def visitVariable(tree: VariableTree, r: Boolean) {
            val elem = TU.elementFromDeclaration(tree)
            addType(referencedElements, elem.asType)
            super.visitVariable(tree, r)
        }
    }
    
    class ElementVisitor(ttf: TranslateTypeFactory) extends ElementScanner6[Unit, Unit] {
        val referencedElements = MutableSet.empty[Element]

        override def visitExecutable(elem: ExecutableElement, v: Unit) {
            referencedElements += EU.enclosingClass(elem)
            addTypesOfAnnotations(referencedElements, elem)
            addType(referencedElements, elem.getReturnType)
            super.scan(elem.getParameters, ())
        }
        
        override def visitType(elem: TypeElement, v: Unit) {
            log.indented("visitType(%s)", elem) {
                elem.getInterfaces.foreach(addType(referencedElements, _))
                addType(referencedElements, elem.getSuperclass)
                
                // Look for annotation types annotated by DefinesGhost
                if(elem.getKind == EK.ANNOTATION_TYPE) {
                    ttf.definedGhost(elem).foreach { case (tm, b) => 
                        addType(referencedElements, tm) }
                } else {
                    addTypesOfAnnotations(referencedElements, elem)                    
                }

                // n.b.: we visit only the (non-private) enclosed fields; 
                // if any of the methods are actually invoked, we'll visit 
                // them then.  This helps to keep the size of the closure under control.  
                // We'd prefer not to visit the fields either, but they may
                // be referenced from paths and other things not visible to us.
                EF.fieldsIn(elem.getEnclosedElements).foreach { velem =>
                    if(!velem.getModifiers.contains(Modifier.PRIVATE)) {
                        log("Including field: %s", velem)
                        referencedElements += velem
                    } else {                        
                        log("Skipping private field: %s", velem)
                    }
                }
            }
            
            // don't visit other enclosed elements
        }
        
        override def visitTypeParameter(elem: TypeParameterElement, v: Unit) {
            addTypesOfAnnotations(referencedElements, elem)
            elem.getBounds.foreach(addType(referencedElements, _))
        }
        
        override def visitVariable(elem: VariableElement, v: Unit) {
            addTypesOfAnnotations(referencedElements, elem)
            referencedElements += EU.enclosingClass(elem)
            addType(referencedElements, elem.asType)
        }
    }
    
    def referenceClosure(ttf: TranslateTypeFactory, tree: ClassTree): MutableSet[Element] = {
        def addElements(
            resultSet: MutableSet[Element],
            newSet: Iterable[Element]
        ): MutableSet[Element] = {
            val oldSize = resultSet.size
            resultSet ++= newSet
            if(resultSet.size == oldSize) {
                resultSet
            } else {
                val ev = new ElementVisitor(ttf)
                newSet.foreach { newElem =>
                    log("Scanning %s", newElem)
                    ev.scan(newElem, ())
                }
                addElements(resultSet, ev.referencedElements)
            }
        }
        
        indexLog.indented("referenceClosure") {
            val tv = new TreeExplorer(ttf)
            val resultSet = MutableSet.empty[Element]
            tree.accept(tv, true)
            addElements(resultSet, tv.referencedElements)
            
            // Ensure that elements on which our analysis relies are present:
            addElements(resultSet, List(
                ttf.wke.Interval.elem,
                ttf.wke.Interval.field("start"),
                ttf.wke.Interval.field("end"),
                ttf.wke.Point.elem,
                ttf.wke.Guard.elem
            ))
        }
    }
    
}