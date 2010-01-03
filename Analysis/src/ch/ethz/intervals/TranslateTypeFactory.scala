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
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import java.util.{List => jList}
import Util._
import quals.DefinesGhost
import ch.ethz.intervals.quals.Constructor

class TranslateTypeFactory(
    tctx: TranslateContext,
    checker: IntervalChecker,
    root: CompilationUnitTree
) extends AnnotatedTypeFactory(checker, root) {
    import tctx.log
    
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

    // All supertypes, class first then interfaces.
    def directSupertys(telem: TypeElement) =
        telem.getSuperclass :: telem.getInterfaces
        
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
            directSupertys(elem).foldLeft(s)(addAncestorsOrSelf)
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
            } else
                GhostAnnValue(f(elem), value)
        }
    }
    
    def addGhostFieldsGivenValueInAnnotations(
        m0: Map[ir.FieldName, String], 
        ams: List[AnnotationMirror]
    ): Map[ir.FieldName, String] =
        ams.map(categorizeGhostAnnot).foldLeft(m0) { 
            case (m, GhostAnnNone) => m
            case (m, GhostAnnDecl(_, _)) => m
            case (m, GhostAnnValue(f, s)) => m + Pair(f, s)
        }    
        
    def ghostFieldsGivenValueInAnnotations(ams: List[AnnotationMirror]) =
        addGhostFieldsGivenValueInAnnotations(Map.empty, ams)
    
    def addGhostFieldsGivenValueOnElem(
        m0: Map[ir.FieldName, String], 
        elem0: Element
    ): Map[ir.FieldName, String] = 
        addGhostFieldsGivenValueInAnnotations(m0, elem0.getAnnotationMirrors)
    
    def ghostFieldsGivenValueOnElem(elem: Element) =
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
    
    def ghostFieldsDeclaredOnElem(elem: Element) =
        addGhostElemsDeclaredOnElem(Map.empty, elem)
    
    def addGhostFieldsDeclaredOnElemAndSuperelems(env: TranslateEnv)(
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
    
    def ghostFieldsDeclaredOnElemAndSuperelems(elem: Element) =
        addGhostElemsDeclaredOnElemAndSuperelems(Map.empty, elem)
    
    def ghostFieldsDeclaredOnAnntyAndSupertypes(annty: AnnotatedTypeMirror) =
        elemOfAnnty(annty).foldLeft(Map.empty)(addGhostElemsDeclaredOnElemAndSuperelems)
    
    // ___ Environment ______________________________________________________
    
    case class TranslateEnv(
        pos: Position,
        lvs: Map[String, (ir.Path, AnnotatedTypeMirror)],
        m_defaultWghosts: Map[ir.FieldName, ir.WcPath]
    ) {
        def setPos[X <: Positional](v: X) = {
            v.setPos(pos)
            v
        }
    }
    
    val emptyEnv = TranslateEnv(
        Map.empty
    )
    
    /// Executes g and restores the old environment afterwards:
    def savingEnv[R](g: => R): R = {
        val oldEnv = env
        try { g } finally { env = oldEnv }
    }

    // ___ Parsing __________________________________________________________
    //
    // Parsing uses the environment to resolve local variables etc.
    
    case class ParsePath(p: ir.Path, annty: AnnotatedTypeMirror)
    
    class AnnotParser(env: TranslateEnv) extends BaseParser {
        def pp = (
            ident           ^^ { case s => startPath(s) }
        |   pp~"."~ident    ^^ { case pp~_~s => extendPath(pp, s) }
        )
        
        def p = pp          ^^ { case pp => pp.p }

        def startPath(id: String): ParsePath = {
            env.lvs.get(id) match {
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
        
        def path(s: String): ir.Path = parseToResult(p)(s)
        def wpath(s: String): ir.WcPath = parseToResult(wp)(s)        
    }
        
    // ___ Translating types ________________________________________________
    
    def erasedTy(ty: TypeMirror): ir.ClassName =
        ty.getKind match {
            case TK.DECLARED => 
                val telem = ty.asInstanceOf[DeclaredType].asElement.asInstanceOf[TypeElement]
                tctx.classAttrs(telem)
                
            case TK.ARRAY =>
                ir.c_array
            
            case tk if tk.isPrimitive =>
                ir.c_scalar

            case _ => // Just treat the other types as VOID (for example, error type etc).
                ir.c_void
        }
        
    def wghosts(env: Environment)(annty: AnnotatedTypeMirror) = {
        // Find and parse the explicit annotations given on the type:
        val m_annty_str = ghostFieldsGivenValueInAnnotations(annty.getAnnotations.toList)
        val m_annty_wp = m_annty_str.transform { case (_, v) => AnnotParser(env).wpath(v) }
        
        // Find the default annotations that are relevant to this type:
        val fs_relevant = ghostFieldsDeclaredOnAnntyAndSupertypes(annty)
        val m_default = env.m_defaultWghosts.filter { case (f, _) => fs_relevant(f) }
        
        // Combine the two, giving precedence to the explicit annotations:
        val m_comb = m_default ++ m_annty_wp        
        m_comb.toList.map { case (f, wp) => env.setPos(ir.WcGhost(f, wp)) }
    }
    
    def wtref(env: Environment)(annty: AnnotatedTypeMirror) = {
        val c = erasedTy(annty.getUnderlyingType)
        annty.getKind match {
            case TK.DECLARED => {
                val tattrs = 
                    if(annty.hasAnnotation(classOf[Constructor])) ir.ctorAttrs
                    else ir.noAttrs                
                ir.TypeRef(c, wghosts(annty), tattrs)
            }
        
            case TK.ARRAY =>
                ir.TypeRef(c, wghosts(annty), ir.noAttrs)
                        
            case _ =>
                ir.TypeRef(c, Map.empty, ir.noAttrs)            
        }            
    }
    
    // ___ Translating the class interface __________________________________
    
    def classAttrs(telem: TypeElement) = telem.getKind match {
        case EK.INTERFACE | EK.ANNOTATION_TYPE => ir.interfaceAttrs
        case _ => ir.noAttrs
    }

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
                
                val ghostDecls = ghostFieldsDeclaredOnElem(telem).map { case (f, s) =>
                    ir.GhostFieldDecl()
                }
        
                val ghosts = ghostFieldsGivenValueOnElem(telem).map { case (f, s) =>
                    ir.Ghost(f, parser.path(s))
                }
        
                ir.ClassDecl(
                    /* Attrs:   */  classAttrs(telem),
                    /* Name:    */  tctx.className(telem),
                    /* Extends: */  directSupertys(telem).flatMap(translateTy(_).toList)
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