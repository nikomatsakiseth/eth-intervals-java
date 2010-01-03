package ch.ethz.intervals

import com.sun.source.tree._
import com.sun.source.tree.Tree.{Kind => TRK}
import checkers.types.AnnotatedTypeMirror
import checkers.types.AnnotatedTypeMirror._
import checkers.types.AnnotatedTypeFactory
import checkers.util.{ElementUtils => EU}
import checkers.util.{TreeUtils => TU}
import checkers.util.{AnnotationUtils => AU}
import checkers.util.AnnotationUtils.AnnotationBuilder
import javax.lang.model.element._
import javax.lang.model.`type`._
import javax.lang.model.element.{ElementKind => EK}
import javax.lang.model.`type`.{TypeKind => TK}
import javax.lang.model.util.{ElementFilter => EF}
import scala.collection.jcl.Conversions._
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.util.{List => jList}
import Util._
import quals.DefinesGhost

class TranslateContext(
    checker: IntervalChecker,
    root: CompilationUnitTree
) extends AnnotatedTypeFactory(checker, root) {
    
    val log = new Log.TmpHtmlLog()
    val cds = new ListBuffer[ir.ClassDecl]()
    
    // ___ Misc. Helpers ____________________________________________________
    
    // Returns 'elem' if it is a class, otherwise the class enclosing 'elem' (or null)
    def closestClass(elem: Element): TypeElement = EU.enclosingClass(elem)

    // Returns 'elem' if it is a package, otherwise the package enclosing 'elem'
    def closestPackage(elem: Element): PackageElement = elem.getKind match {
        case EK.PACKAGE => elem.asInstanceOf[PackageElement]
        case _ => closestPackage(elem.getEnclosingElement)
    }
        
    // Converts from a type element to an ir.ClassName
    def className(typeElem: TypeElement) =
        ir.ClassName(qualName(typeElem))
        
    // Extracts the 'fldName'() argument from 'am' as a String.
    def annField(am: AnnotationMirror, fldName: String) = 
        AU.parseStringValue(am, fldName)
        
    // Extracts the value() argument from 'am' as a String.
    def annValue(am: AnnotationMirror) = 
        annField(am, "value")
        
    def elemKind(annty: AnnotatedTypeMirror): Option[ElementKind] =
        annty.getKind match {
            case TK.DECLARED => Some(annty.getUnderlyingType.asInstanceOf[DeclaredType].asElement.getKind)
            case _ => None
        }
        
    def elemOfType(ty: TypeMirror): Option[Element] =
        ty.getKind match {
            case TK.DECLARED => Some(ty.asInstanceOf[DeclaredType].asElement)
            case _ => None
        }
        
    def elemOfAnnty(annty: AnnotatedTypeMirror): Option[Element] =
        elemOfType(annty.getUnderlyingType)
        
    def addAncestorsOrSelf(s: Set[Element], ty: TypeMirror): Set[Element] = {
        elemOfType(ty).map(ancestorsOrSelf(s, _)).getOrElse(s)
    }
    
    def addAncestorsOrSelf(s: Set[Element], elem: Element): Set[Element] = {
        if(!s(elem)) {
            val s1 = s + elem
            (elem.getSuperclass :: elem.getInterfaces).foldLeft(s)(addAncestorsOrSelf)
        }
    }
    
    def ancestorsOrSelf(elem: Element) = addAncestorsOrSelf(Set.empty, elem)
    
    def superElems(elem: Element) = ancestorsOrSelf(elem) - elem
    
    def isInterfaceType(annty: AnnotatedTypeMirror) =
        elemKind(annty) == Some(ElementKind.INTERFACE)
        
    def isClassType(annty: AnnotatedTypeMirror) =
        elemKind(annty) == Some(ElementKind.CLASS)
        
    // Converts an Element to the ir.FieldName it represents.    If Element
    // is a genuine field, this is just the name of that field.  If Element
    // is an annotation (i.e., an object parameter), this is the full qualified
    // name of that annotation class.
    def f(elem: Element): ir.FieldName =
        elem.getKind match {
            case EK.FIELD => ir.FieldName(elem.getSimpleName().toString())
            case _ => ir.FieldName(qualName(elem))              
        }
        
    // Given a field name like "a.b.c", returns "c"
    def shortFieldName(f: ir.FieldName) = {
        if(!f.name.contains(".")) 
            f.name
        else {
            val idx = f.name.lastIndexOf('.')
            f.name.substring(idx+1)
        }
    }
    
    // Constructs an ir.MethodName from the combination of a Java method name
    // and its argument types.  Keep synchronized with m() below.
    def m(methodName: String, argClasses: List[Class[_]]): ir.MethodName = {
        val sb = new StringBuilder()
        sb.append(methodName)
        
        def argName(c: Class[_]): String = c match {
            case c if c.isPrimitive =>
                c.getSimpleName
            case c if c.isArray =>
                argName(c.getComponentType) + "[]"
            case c =>
                c.getName
        }        
        val argNames = argClasses.map(argName)      
        sb.append("(")
        sb.append(",".join(argNames)) // kinda' inefficient...
        sb.append(")")
        
        ir.MethodName(sb.toString)
    }    
    
    def findField(annty_owner: AnnotatedTypeMirror, s: String): Option[AnnotatedTypeMirror] =
        elemOfAnnty(annty_owner) match {
            case None => None
            case Some(elem_owner) =>
                val elems_owner_fields = EF.fieldsIn(elements.getAllMembers(elem_owner))
                val nm = elements.getName(s)
                elems_owner_fields.find(_.getSimpleName == nm).map(elem_field =>
                    atypes.asMemberOf(annty_owner, elem_field))
        }
        
    // ___ Ghost Fields _____________________________________________________
    
    sealed abstract class GhostAnn
    sealed case object GhostAnnNone extends GhostCategory
    sealed case object GhostAnnDecl(f: ir.FieldName, annty: AnnotatedTypeMirror) extends GhostCategory
    sealed case object GhostAnnValue(f: ir.FieldName, value: String) extends GhostCategory
    
    val defGhostClass = ClassOf[DefinesGhost]
    
    def categorizeGhostAnnot(am: AnnotationMirror) = {
        val elem = am.getAnnotationType.asElement
        if(elem.getAnnotation(defGhostClass) == null) GhostCategoryNone
        else {
            val elem = am.getAnnotationType.asElement
            val value = annValue(am)
            if(value == "") {
                val ghostCls = elem.getAnnotationType(defGhostClass).cls
                val ghostElem = elements.getTypeElement(ghostCls.getName)
                val ghostAnnty = getAnnotatedType(ghostElem)
                GhostAnnDecl(f(elem), ghostAnnty))
            } else {
                GhostAnnValue(f(elem), value)
            }
        }
    }
    
    def addGhostFieldsGivenValueOnElem(
        m0: Map[ir.FieldName, String], 
        elem0: Element
    ): Map[ir.FieldName, String] = 
        elem0.getAnnotationMirrors.map(categorizeGhostAnnot).foldLeft(m2) { 
            case (m, GhostAnnNone) => m
            case (m, GhostAnnDecl(_, _)) => m
            case (m, GhostAnnValue(f, s)) => m + Pair(f, s)
        }    
    
    def ghostFieldsGivenValueOnElem(elem: Element): Map[ir.FieldName, AnnotatedTypeMirror] = 
        addGhostFieldsGivenValueOnElem(Map.empty, elem)
        
    def addGhostFieldsDeclaredOnElem(
        m0: Map[ir.FieldName, AnnotatedTypeMirror], 
        elem0: Element
    ): Map[ir.FieldName, AnnotatedTypeMirror] = 
        elem0.getAnnotationMirrors.map(categorizeGhostAnnot).foldLeft(m2) { 
            case (m, GhostAnnNone) => m
            case (m, GhostAnnDecl(f, annty)) => m + Pair(f, annty)
            case (m, GhostAnnValue(f, _)) => m - f
        }    
    
    def ghostFieldsDeclaredOnElem(elem: Element): Map[ir.FieldName, AnnotatedTypeMirror] = 
        addGhostElemsDeclaredOnElem(Map.empty, elem)
    
    def addGhostFieldsDeclaredOnElemAndSuperelems(
        m0: Map[ir.FieldName, AnnotatedTypeMirror], 
        elem0: Element
    ): Map[ir.FieldName, AnnotatedTypeMirror] = {
        elem0.getKind match {
            case EK.CLASS | EK.INTERFACE | EK.ENUM | EK.ANNOTATION_TYPE =>
                // Extract decls from supertypes:
                val m1 = elemOfAnnty(elem0.getSuperclass).foldLeft(m0)(addGhostFieldsDeclaredOnElem)
                val interfaceElems = elem0.getInterfaces.flatMap(elemOfAnnty(_).toList)
                val m2 = interfaceElems.foldLeft(m1)(addGhostFieldsDeclaredOnElem)
                
                // Process annotations on this class:
                addGhostFieldsDeclaredOnElem(m2, elem0)
                
            case _ =>
                Map.empty
        }
    }
    
    def ghostFieldsDeclaredOnElemAndSuperelems(elem: Element): Map[ir.FieldName, AnnotatedTypeMirror] = 
        addGhostElemsDeclaredOnElemAndSuperelems(Map.empty, elem)
    
    // ___ Parsing __________________________________________________________
    
    case class ParsePath(p: ir.Path, annty: AnnotatedTypeMirror)
    
    // Parsing user text strings always takes place in a lexical context.
    // This allows us to convert short names to fully-qualified ones.
    class ParsingScope(
        lvs: Map[String, (ir.Path, AnnotatedTypeMirror)]
    ) {
        
        def startPath(id: String): ParsePath = {
            lvs.get(id) match {
                case Some((p, annty)) => ParsePath(p, annty)
                case None => throw ir.IrError("intervals.no.such.variable", id)
            }
        }
        
        def extendPath(pp: ParsePath, id: String) = {
            val declGhosts = elemOfAnnty(pp.annty).map(ghostFieldsDeclaredOnElemAndSuperelems).getOrElse(Map.empty)            
            val f_id = ir.FieldName(id)
            declGhosts.get(f_id) match {
                // Exact match:
                case Some(annty) => ParsePath(pp.p + f_id, annty)
                
                // Search for a non-ambigious short-name match:
                case None =>
                    declGhosts.keySet.filter(f_g => shortFieldName(f_g) == f_id) match {                        
                        // Single match: use the ghost
                        case Set(f_g) => ParsePath(pp.p + f_g, declGhosts(f_g))
                        
                        // No matchs, search for a real field with that name:
                        case Set() => 
                            findField(p.annty, id) match {
                                case Some(annty_id) => ParsePath(pp.p + f_id, annty_id)                                    
                                case None =>throw new ir.IrError("intervals.no.such.field", f_id)
                            }
                            
                        // Multiple matches, error:
                        case fs => throw ir.IrError("intervals.ambig.ghost", fs.mkString(", "))
                    }
            }
        }
        
    }
    
    class AnnotParser(scope: ParsingScope) extends BaseParser {
        def pp = (
            ident       ^^ { case s => scope.startPath(s) }
        |   pp~"."~ident ^^ { case pp~_~s => scope.extendPath(pp, s) }
        )
        
        def path(s: String): ir.Path = {
            parse(pp)(s) match {
                case Success(pp, _) => pp.p
                case n: NoSuccess => throw new ir.IrError(intervals.parse.error, n.toString)
            }            
        }
    }
    
    def classAttrs(telem: TypeElement) = telem.getKind match {
        case EK.INTERFACE | EK.ANNOTATION_TYPE => ir.interfaceAttrs
        case _ => ir.noAttrs
    }
    
    // Translates an erased type to an IR erased type.
    def translateTy(ty: TypeMirror): Option[ir.ClassName] =
        ty.getKind match {
            case TK.DECLARED => 
                val telem = ty.asInstanceOf[DeclaredType].asElement.asInstanceOf[TypeElement]
                Some(tctx.classAttrs(telem))
                
            case TK.ARRAY =>
                Some(ir.c_array)
            
            case TK.VOID =>
                Some(ir.c_void)
            
            case TK.BOOLEAN | TK.BYTE | TK.CHAR | TK.DOUBLE | TK.FLOAT | TK.INT | TK.LONG | TK.SHORT =>
                Some(ir.c_scalar)
            
            case _ => None
        }
        
    def superTys(telem: TypeElement) =
        telem.getSuperclass :: telem.getInterfaces
        
    def dummyClassDecl(telem: TypeElement) = 
        ir.ClassDecl(
            /* Attrs:   */  classAttrs(telem),
            /* Name:    */  tctx.className(telem),
            /* Extends: */  List(),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(MethodDecl(
                    /* attrs:  */ ctorAttrs,
                    /* wt_ret: */ t_void, 
                    /* name:   */ m_init, 
                    /* args:   */ List(),
                    /* reqs:   */ List(),
                    /* blocks: */ Array(
                        Block(
                            /* args:  */ List(),
                            /* stmts: */ List(ir.StmtSuperCtor(m_init, List())),
                            /* goto:  */ List()
                        )
                    )
                )),
            /* Fields:  */  List(),
            /* Methods: */  List()
        )
    
    // Returns a partial class decl. including ctor, fields, methods
    def headerClassDecl(telem: TypeElement): ir.ClassDecl = 
        log.indentedRes("headerClassDecl: %s", telem) {
            at(telem, dummyClassDecl(telem)) {        
                val parser = tctx.AnnotParser(scope)
                
                val ghostDecls = 
        
                val ghosts = ghostFieldsGivenValueOnElem(telem).map { case (f, s) =>
                    ir.Ghost(f, parser.path(s))
                }
        
                ir.ClassDecl(
                    /* Attrs:   */  classAttrs(telem),
                    /* Name:    */  tctx.className(telem),
                    /* Extends: */  superTys(telem).flatMap(translateTy(_).toList)
                    /* Ghosts:  */  ghosts,
                    /* Reqs:    */  List(), // TODO
                    /* Ctor:    */  List(), // Not included in header.
                    /* Fields:  */  List(), // Not included in header. 
                    /* Methods: */  List()  // Not included in header.
                )
            }            
        }
        
    // ___ AnnotatedTypeFactory methods _____________________________________
    
    override def annotateInheritedFromClass(atm: AnnotatedTypeMirror) {
        // IMPORTANT-- Do NOT use killExtraAnnots() here, because it interferes
        // with array processing!    (When processing arrays, annotations
        // sometimes get stuck in inappropriate places)
    }
    
    def postDirectSuperType(superAtm: AnnotatedTypeMirror) {
        // if the class Object is not specified explicitly, its type is supplied
        // by invoking fromElement(Object), and so we get a bogus type like
        // @Identity Object, when what we really wanted is simply Object.    
        // So, for Object, we purge ann like @Identity.
/*        if(superAtm.getKind == TK.DECLARED) {
            val superDatm = superAtm.asInstanceOf[AnnotatedDeclaredType]
            if(wke.`object`.elem == superDatm.getUnderlyingType.asElement) {
                log("Purging empty annotations from ref to Object")
                def isEmptyAnnot(am: AnnotationMirror) =
                    am.getElementValues.isEmpty || annValue(am) == ""
                val emptyAnnots = superAtm.getAnnotations.filter(isEmptyAnnot)
                emptyAnnots.foreach(superDatm.removeAnnotation(_))
                log("Result: %s", superDatm)
            }
        }  */      
    }
    
    override def postDirectSuperTypes(atm: AnnotatedTypeMirror, superAtms: jList[_ <: AnnotatedTypeMirror]) {
        log.indented("postDirectSuperTypes(%s, %s)", atm, superAtms) {          
            convertList(superAtms).foreach(postDirectSuperType)
        }
    }
    
    override def annotateImplicit(elem: Element, atm: AnnotatedTypeMirror) {
        log.indented("annotateImplicit[Element](%s, %s)", elem, atm) {
            elem.getKind match {
                case EK.CLASS | EK.INTERFACE | EK.ENUM | EK.ANNOTATION_TYPE =>
/*                    val objParams = objParametersInScope(elem)
                    atm.clearAnnotations
                    objParams.foreach { case objParam => log.indented("default annot: %s", objParam) {
                        log("env=%s", env)
                        val ab = new AnnotationBuilder(env, objParam.name)
                        ab.setValue("value", shortFieldName(objParam))
                        val am = ab.build()
                        atm.addAnnotation(am)
                    } }*/
                case _ =>
            }
            log("Result=%s", atm)
        }
    }

    override def annotateImplicit(tree: Tree, atm: AnnotatedTypeMirror) {
        log.indented("annotateImplicit[Tree](%s, %s)", tree, atm) {
            tree.getKind match {
                case TRK.CLASS =>
                    val elem = TU.elementFromDeclaration(tree.asInstanceOf[ClassTree])
                    annotateImplicit(elem, atm)
                case _ =>
            }
            log("Result=%s", atm)
        }
    }       
}