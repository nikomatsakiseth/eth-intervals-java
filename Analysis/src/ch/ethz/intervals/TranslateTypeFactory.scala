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
import javax.lang.model.`type`._
import javax.lang.model.element.{ElementKind => EK}
import javax.lang.model.`type`.{TypeKind => TK}
import javax.lang.model.util.{ElementFilter => EF}
import scala.collection.JavaConversions._
import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import java.util.{List => jList}
import Util._
import quals.DefinesGhost
import ch.ethz.intervals.quals._
import ch.ethz.intervals.log.LogStack

class TranslateTypeFactory(
    logStack: LogStack,
    checker: IntervalsChecker,
    root: CompilationUnitTree
) extends AnnotatedTypeFactory(checker, root) {
    import logStack.log
    import logStack.indexLog
    
    val processingEnvironment = env
    val wke = new WellKnownElements(
        processingEnvironment.getElementUtils, 
        processingEnvironment.getTypeUtils)
    def annty_interval = getAnnotatedType(wke.Interval.elem) // safe only b/c it has no unbound ghosts
    
    // ___ Positions ________________________________________________________
    
    case object NullPosition extends DummyPosition {
        def reportObject = null
        def rewrite(s: String) = s
        override def toString = "NullPosition(%s)"
    }
    
    case class TreePosition(tree: Tree, rewriteFunc: (String => String)) extends DummyPosition {
        def reportObject = tree
        def rewrite(s: String) = rewriteFunc(s)
        override def toString = "TreePosition(%s)".format(tree)
    }
    
    case class ElementPosition(elem: Element) extends DummyPosition {
        def reportObject = elem
        def rewrite(s: String) = s
        override def toString = "ElementPosition(%s)".format(qualName(elem))
    }
    
    def reportErrorsFromLogStack() {
        logStack.errors.foreach { e =>
            log("Error: %s", e)        
            val pos = e.pos.asInstanceOf[DummyPosition]
            checker.report(Result.failure(e.msg, e.args: _*), pos.reportObject)            
        }
    }
    
    def at[R](p: DummyPosition, default: => R)(func: => R) =
        logStack.at(new DummyPositional(p, "At"), default)(func)

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
        if(telem.getKind.isInterface) {
            telem.getInterfaces.toList match {
                case List() => List(wke.Object.ty)
                case lst => lst
            }
        } else if (telem == wke.Object.elem) {
            telem.getInterfaces.toList
        } else {
            telem.getSuperclass :: telem.getInterfaces.toList            
        }
        
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
        
    def elemOfType(ty: TypeMirror): Option[TypeElement] =
        ty.getKind match {
            case TK.DECLARED => Some(ty.asInstanceOf[DeclaredType].asElement.asInstanceOf[TypeElement])
            case _ => None
        }
        
    def elemOfAnnty(annty: AnnotatedTypeMirror): Option[TypeElement] =
        elemOfType(annty.getUnderlyingType)
        
    def isInterfaceType(annty: AnnotatedTypeMirror) =
        elemKind(annty) == Some(ElementKind.INTERFACE)
        
    def isClassType(annty: AnnotatedTypeMirror) =
        elemKind(annty) == Some(ElementKind.CLASS)
        
    def annotationMirrorOfElement(elem: Element, tm: TypeMirror) = {
        elem.getAnnotationMirrors.find { am =>
            types.isSameType(am.getAnnotationType, tm)
        }
    }
        
    // Converts an Element to the ir.FieldName it represents.    If Element
    // is a genuine field, this is just the name of that field.  If Element
    // is an annotation (i.e., an object parameter), this is the full qualified
    // name of that annotation class.
    def f(elem: Element): ir.FieldName =
        elem.getKind match {
            case EK.FIELD => ir.FieldName(elem.getSimpleName().toString())
            case _ => ir.FieldName(qualName(elem))              
        }
        
    def lv(velem: VariableElement): ir.VarName =
        ir.VarName(velem.getSimpleName.toString)
        
    // Given a field name like "a.b.c", returns "c"
    def shortFieldName(f: ir.FieldName) = {
        if(!f.name.contains(".")) 
            f.name
        else {
            val idx = f.name.lastIndexOf('.')
            f.name.substring(idx+1)
        }
    }
    
    def m(methodName: String, argTypes: List[TypeMirror]): ir.MethodName = {
        val sb = new StringBuilder()
        sb.append(methodName)
        
        def argName(tm: TypeMirror): String = tm.getKind match {
            case TK.ARRAY => argName(tm.asInstanceOf[ArrayType].getComponentType) + "[]"
            case TK.DECLARED => qualName(tm.asInstanceOf[DeclaredType].asElement)
            case _ => tm.toString
        }
        val argNames = argTypes.map(argName)
        sb.append("(")
        sb.append(",".join(argNames)) // kinda' inefficient...
        sb.append(")")
        
        ir.MethodName(sb.toString)        
    }

    def m(methodName: String, etm: ExecutableType): ir.MethodName = {
        m(methodName, etm.getParameterTypes.toList)
    }
    
    def m(eelem: ExecutableElement): ir.MethodName = {
        m(eelem.getSimpleName.toString, eelem.asType.asInstanceOf[ExecutableType])
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
    case object GhostAnnNone extends GhostAnn
    sealed case class GhostAnnDecl(f: ir.FieldName, annty: AnnotatedTypeMirror) extends GhostAnn
    sealed case class GhostAnnValue(f: ir.FieldName, value: String) extends GhostAnn
    
    def categorizeGhostAnnot(am: AnnotationMirror) =
        log.indented("categorizeGhostAnnot(%s)", am) {
            val elem = am.getAnnotationType.asElement
            if(elem.getAnnotation(classOf[DefinesGhost]) == null) GhostAnnNone
            else {
                val value = annValue(am)
                if(value == "") {
                    val ghostTypeString = elem.getAnnotation(classOf[DefinesGhost]).`type`
                    val ghostAnnty = AnnTyParser(ghostTypeString)
                    GhostAnnDecl(f(elem), ghostAnnty)
                } else
                    GhostAnnValue(f(elem), value)
            }            
        }
    
    // Higher-level function that folds elemFunc over
    // every element in 'ty' and its supertypes, progressively
    // adding to the map 'm0' and returning the final result.
    def addTyAndSupertypes[K,V](
        elemFunc: ((Map[K,V], Element) => Map[K,V])
    )(
        m0: Map[K,V],
        ty: TypeMirror
    ): Map[K,V] =
        elemOfType(ty).foldLeft(m0)(addElemAndSuperelems(elemFunc))        
        
    // Higher-level function that folds elemFunc over
    // elem and the elements of its supertypes, progressively
    // adding to the map 'm0' and returning the final result.
    def addElemAndSuperelems[K,V](
        elemFunc: ((Map[K,V], Element) => Map[K,V])
    )(
        m0: Map[K,V],
        elem: Element
    ): Map[K,V] = {
        elem.getKind match {
            case EK.CLASS | EK.INTERFACE | EK.ENUM | EK.ANNOTATION_TYPE =>
                // Extract decls from supertypes:
                val telem = elem.asInstanceOf[TypeElement]
                val m1 = addTyAndSupertypes(elemFunc)(m0, telem.getSuperclass)
                val m2 = telem.getInterfaces.foldLeft(m1)(addTyAndSupertypes(elemFunc))
                
                // Process annotations on this class:
                elemFunc(m2, telem)
                
            case _ =>
                Map.empty
        }        
    }
    
    // ______ Bound Ghost Fields ____________________________________________
    //
    // Bound ghost fields are those ghost fields given a value on the class
    // declaration.  For example, the class Foo: 
    //   @Creator("this.constructor")
    //   class Foo { ... }
    // binds the ghost field Creator to "this.constructor".

    def addGhostFieldsBoundInAnnotations(
        m0: Map[ir.FieldName, String], 
        ams: List[AnnotationMirror]
    ): Map[ir.FieldName, String] =
        ams.map(categorizeGhostAnnot).foldLeft(m0) { 
            case (m, GhostAnnNone) => m
            case (m, GhostAnnDecl(_, _)) => m
            case (m, GhostAnnValue(f, s)) => m + Pair(f, s)
        }    
        
    def ghostFieldsBoundInAnnotations(ams: List[AnnotationMirror]) =
        log.indented("ghostFieldsBoundInAnnotations(%s)", ams) {
            addGhostFieldsBoundInAnnotations(Map.empty, ams)
        }        
    
    def addGhostFieldsBoundOnElem(
        m0: Map[ir.FieldName, String], 
        elem0: Element
    ): Map[ir.FieldName, String] = 
        addGhostFieldsBoundInAnnotations(m0, elem0.getAnnotationMirrors.toList)
    
    def ghostFieldsBoundOnElem(elem: Element) =
        log.indented("ghostFieldsBoundOnElem(%s)", elem) {
            addGhostFieldsBoundOnElem(Map.empty, elem)
        }
        
    def ghostFieldsBoundOnElemAndSuperelems(elem: Element) =
        log.indented("ghostFieldsBoundOnElemAndSuperelems(%s)", elem) {
            addElemAndSuperelems(addGhostFieldsBoundOnElem)(Map.empty, elem)
        }
                
    def ghostFieldsBoundOnTyAndSupertypes(ty: TypeMirror) =
        log.indented("ghostFieldsBoundOnTyAndSupertypes(%s)", ty) {
            addTyAndSupertypes(addGhostFieldsBoundOnElem)(Map.empty, ty)
        }
        
    // ______ Declared Ghost Fields _________________________________________
    //
    // Declared ghost fields are just all ghost fields declared.  For example,
    // the class Foo:
    //     @SomeField class Foo { ... }
    // declares the ghost field SomeField.  It also inherits the ghost field
    // Creator, declared on its supertype Object.

    def addGhostFieldsDeclaredOnElem(
        m0: Map[ir.FieldName, AnnotatedTypeMirror], 
        elem0: Element
    ): Map[ir.FieldName, AnnotatedTypeMirror] = 
        log.indented("addGhostFieldsDeclaredOnElem(%s)", elem0) {
            elem0.getAnnotationMirrors.map(categorizeGhostAnnot).foldLeft(m0) { 
                case (m, GhostAnnNone) => m
                case (m, GhostAnnDecl(f, annty)) => m + Pair(f, annty)
                case (m, GhostAnnValue(_, _)) => m
            }    
        }
    
    def ghostFieldsDeclaredOnElem(elem: Element) =
        log.indented("ghostFieldsDeclaredOnElem(%s)", elem) {
            addGhostFieldsDeclaredOnElem(Map.empty, elem)
        }
                
    def ghostFieldsDeclaredOnElemAndSuperelems(elem: Element) =
        log.indented("ghostFieldsDeclaredOnElemAndSuperelems(%s)", elem) {
            addElemAndSuperelems(addGhostFieldsDeclaredOnElem)(Map.empty, elem)
        }
    
    def ghostFieldsDeclaredOnTyAndSupertypes(ty: TypeMirror) =
        log.indented("ghostFieldsDeclaredOnTyAndSupertypes(%s)", ty) {
            addTyAndSupertypes(addGhostFieldsDeclaredOnElem)(Map.empty, ty)
        }
    
    // ______ Unbound Ghost Fields __________________________________________
    //
    // An unbound ghost field is one that is declared but not yet bound.
    
    def unboundGhostFieldsDeclaredOnTyAndSupertypes(ty: TypeMirror) =
        log.indented("unboundGhostFieldsDeclaredOnTyAndSupertypes(%s)", ty) {
            val m_decl = ghostFieldsDeclaredOnTyAndSupertypes(ty)
            ghostFieldsBoundOnTyAndSupertypes(ty).foldLeft(m_decl) { case (m, (f, _)) =>
                m - f
            }
        }
    
    // ___ Environment ______________________________________________________
    
    case class TranslateEnv(
        pos: Position,
        m_lvs: Map[String, (ir.Path, AnnotatedTypeMirror)],
        m_defaultWghosts: Map[ir.FieldName, ir.WcPath]
    )
    
    val emptyEnv = TranslateEnv(NullPosition, Map.empty, Map.empty)

    def elemEnv(elem: Element): TranslateEnv = log.indented("elemEnv(%s)", elem) {
        elem.getKind match {
            case EK.PACKAGE =>
                emptyEnv
                
            // ----------------------------------------------------------------------
            // Computing class environment:
            case EK.CLASS | EK.ENUM | EK.INTERFACE | EK.ANNOTATION_TYPE => {
                val telem = elem.asInstanceOf[TypeElement]
                val annty_this = getAnnotatedType(telem)

                // ----------------------------------------------------------------------
                // "Local variables:"
                //
                // At the class level, paths may begin with "this" or any of the fields, 
                // ghost and reified, declared on the class.
                //
                // TODO-- Detect possible aliases among short field names here and do not
                // add to the dictionary.

                var m_lvs = Map.empty[String, (ir.Path, AnnotatedTypeMirror)]

                val elems_fields = EF.fieldsIn(elements.getAllMembers(telem))
                m_lvs = elems_fields.foldLeft(m_lvs) { case (m, elem) =>
                    val f_elem = f(elem)
                    m + Pair(f_elem.name, (f_elem.thisPath, atypes.asMemberOf(annty_this, elem))) 
                }

                val ghostFields = ghostFieldsDeclaredOnElemAndSuperelems(telem)
                m_lvs = ghostFields.foldLeft(m_lvs) { case (m, (f, annty)) =>
                    m + Pair(shortFieldName(f), (f.thisPath, annty)) 
                }

                m_lvs = m_lvs + Pair(ir.lv_this.name, (ir.p_this, annty_this))

                // ----------------------------------------------------------------------
                // Default Ghosts:
                //
                // All in-scope ghost parameters G are automatically supplied to types
                // within (i.e., @G("G") is the default for any in-scope ghost parameter).

                var m_defaultWghosts = Map.empty[ir.FieldName, ir.WcPath]
                m_defaultWghosts = ghostFields.foldLeft(m_defaultWghosts) { case (m, (f, annty)) =>
                    m + Pair(f, f.thisPath)
                }

                TranslateEnv(ElementPosition(telem), m_lvs, m_defaultWghosts)
            }

            // ----------------------------------------------------------------------
            // Computing method environment:
            case EK.CONSTRUCTOR | EK.METHOD => {
                val eelem = elem.asInstanceOf[ExecutableElement]
                val env_cls = elemEnv(eelem.getEnclosingElement)

                // ----------------------------------------------------------------------
                // "Local variables:"
                //
                // Add method arguments to the map of things that can start a path.
                // Each arg x -> (p, annty) where p = x and annty = annty(x)

                var m_lvs = env_cls.m_lvs
                m_lvs += Pair("method", (ir.p_mthd, annty_interval))
                m_lvs = eelem.getParameters.foldLeft(m_lvs) { case (m, velem) =>
                    m + Pair(
                        velem.getSimpleName.toString, 
                        (lv(velem).path, getAnnotatedType(velem)))
                }
                log.map("Local variables", m_lvs)

                // ----------------------------------------------------------------------
                // "Defaults:"
                //
                // Methods do not (currently) affect the default ghosts.  In the future
                // we may allow defaults to be specified on a method level, however.

                var m_defaultWghosts = env_cls.m_defaultWghosts
                log.map("default wghosts", m_defaultWghosts)

                TranslateEnv(ElementPosition(eelem), m_lvs, m_defaultWghosts)                
            }
            
            case _ =>
                elemEnv(elem.getEnclosingElement)
        }            
    }

    // ___ Parsing __________________________________________________________
    //
    // Parsing uses the environment to resolve local variables etc.
    
    case class ParsePath(p: ir.Path, annty: AnnotatedTypeMirror)
    
    val parserLog = log // log is inherited from BaseParser, so create an alias
    class AnnotParser(env: TranslateEnv) extends BaseParser {
        def p = id~rep("."~>f)          ^^ { case s~ss => ss.foldLeft(startPath(s))(extendPath).p }
        
        def startPath(id: String): ParsePath = {
            parserLog("startPath(%s)", id)
            env.m_lvs.get(id) match {
                case Some((p, annty)) => ParsePath(p, annty)
                case None => throw new CheckFailure("intervals.no.such.variable", id)
            }
        }
        
        def extendPath(pp: ParsePath, f_ext: ir.FieldName) = {
            parserLog("extendPath(%s, %s)", pp, f_ext)
            val declGhosts = elemOfAnnty(pp.annty).map(ghostFieldsDeclaredOnElemAndSuperelems).getOrElse(Map.empty)

            // note: We use this semi-awkward 'if/else' construction
            // to work around some bug in the scala compiler that caused
            // its code generator to fail.
            
            if(declGhosts.isDefinedAt(f_ext))
                // Exact match:
                ParsePath(pp.p + f_ext, declGhosts(f_ext))
                
            else f_ext match {
                // Built-in constructor intervals:
                case ir.CtorFieldName(_) => {
                    ParsePath(pp.p + f_ext, annty_interval)                    
                }
                
                // Search for a non-ambigious short-name match:
                case ir.FieldName(str_ext) => {
                    def hasMatchingShortName(f: ir.FieldName) =
                        shortFieldName(f) == str_ext
                    val potentialMatches = declGhosts.keySet.filter(hasMatchingShortName).toList
                    potentialMatches match {
                        // Single match: use the ghost
                        case List(f_g) => 
                            ParsePath(pp.p + f_g, declGhosts(f_g))
                        
                        // No matchs, search for a real field with that name:
                        case List() => 
                            findField(pp.annty, str_ext) match {
                                case Some(annty_ext) => ParsePath(pp.p + f_ext, annty_ext)
                                case None => throw new CheckFailure("intervals.no.such.field", f_ext)
                            }
                            
                        // Multiple matches, error:
                        case fs => 
                            throw new CheckFailure("intervals.ambig.ghost", fs.mkString(", "))
                    }                    
                }
            }
        }
        
        def path(s: String): ir.Path = 
            parserLog.indented("parse path(%s)", s) {
                parseToResult(p)(s)
            }
            
        def wpath(s: String): ir.WcPath = 
            parserLog.indented("parse wpath(%s)", s) {
                parseToResult(wp)(s)
            }
    }
    
    object AnnotParser {
        def apply(env: TranslateEnv) = new AnnotParser(env)
    }
    
    object ElemLookup extends PartialFunction[String, TypeElement] {
        def apply(nm: String) =
            elements.getTypeElement(nm)
            
        def isDefinedAt(nm: String) =
            apply(nm) != null
    }
    
    class AnnTyParser extends BaseParser {
        def p = null // Unused here.
        
        // For parsing AnnotatedTypes:
        //     Eventually could be improved to allow annotations,
        //     use imports, and in numerous other ways.
        def di = repsep(ident, ".")     ^^ { case idents => ".".join(idents) }
        def elem = di                   ^? ( ElemLookup, (nm => "No such type: " + nm) )
        def annty = elem                ^^ { case e => getAnnotatedType(e) }
    }

    object AnnTyParser {
        def typeElement(s: String): TypeElement = 
            parserLog.indented("parse elem(%s)", s) {
                val parser = new AnnTyParser()
                parser.parseToResult(parser.elem)(s)
            }
        
        def apply(s: String): AnnotatedTypeMirror =
            parserLog.indented("parse annty(%s)", s) {
                val parser = new AnnTyParser()
                parser.parseToResult(parser.annty)(s)
            }
    }
    
    // ___ Translating types ________________________________________________
    
    def erasedTy(ty: TypeMirror): ir.ClassName =
        ty.getKind match {
            case TK.DECLARED => 
                val telem = ty.asInstanceOf[DeclaredType].asElement.asInstanceOf[TypeElement]
                className(telem)
                
            case TK.ARRAY =>
                ir.c_array
            
            case tk if tk.isPrimitive =>
                ir.c_scalar

            case _ => // Just treat the other types as VOID (for example, error type etc).
                ir.c_void
        }
        
    def wghosts(env: TranslateEnv)(annty: AnnotatedTypeMirror) = {
        // Find and parse the explicit annotations given on the type:
        val m_annty_str = ghostFieldsBoundInAnnotations(annty.getAnnotations.toList)
        val m_annty_wp = m_annty_str.transform { case (_, v) => AnnotParser(env).wpath(v) }
        
        // Find the default annotations that are relevant to this type:
        val fs_relevant = unboundGhostFieldsDeclaredOnTyAndSupertypes(annty.getUnderlyingType).keySet
        val m_defaultWghosts = env.m_defaultWghosts.filter { case (f, _) => fs_relevant(f) }
        
        // Combine the two, giving precedence to the explicit annotations:
        val m_comb = m_defaultWghosts ++ m_annty_wp        
        m_comb.toList.map { case (f, wp) => ir.WcGhost(f, wp) }
    }
    
    def unconstructed(c_ty: ir.ClassName, oam: Option[AnnotationMirror]) = {
        oam match {
            case None => ir.FullyConstructed
            case Some(am) =>
                val strings = AU.parseStringArrayValue(am, "value")
                val elems = Set(strings.map(AnnTyParser.typeElement): _*)
                ir.PartiallyConstructed(c_ty, elems.map(className))
        }
    }
    
    def wtref(env: TranslateEnv)(annty: AnnotatedTypeMirror) = {
        // XXX Add type arguments.
        log.indented("wtref(%s)", annty) {
            val c = erasedTy(annty.getUnderlyingType)
            val oam = nullToOption(annty.getAnnotation(wke.Unconstructed.cls))
            val uncons = unconstructed(c, oam)
            
            annty.getKind match {
                case TK.DECLARED =>
                    ir.WcClassType(c, wghosts(env)(annty), List(), uncons)

                case TK.ARRAY =>
                    ir.WcClassType(c, wghosts(env)(annty), List(), uncons)

                case _ =>
                    ir.WcClassType(c, List(), List(), uncons)
            }                        
        }
    }
    
    // ___ Requirements _____________________________________________________
    
    def reqs(elem: Element): List[ir.Req] = {
        val parser = AnnotParser(elemEnv(elem))
        
        def subintervalReq(annot: Subinterval) =
            ir.ReqSubintervalOf(
                annot.subinterval.map(parser.path).toList, 
                annot.of.map(parser.path).toList)
        
        def readableReq(annot: Readable) =
            ir.ReqReadableBy(
                annot.value.map(parser.path).toList, 
                annot.by.map(parser.path).toList)
        
        def writableReq(annot: Writable) =
            ir.ReqWritableBy(
                annot.value.map(parser.path).toList, 
                annot.by.map(parser.path).toList)
        
        def happensReq(annot: Happens) =
            ir.ReqHb(
                annot.before.map(parser.path).toList, 
                annot.after.map(parser.path).toList)
        
        // n.b.: The order in which we process the different
        // kinds of requirements CAN be significant.
        // Processing hb first may permit us to realize that
        // a field is constant and thus allow requirements
        // that reference it.
        
        elem.getAnnotation(classOf[Requires]) match {
            case null => List()
            case annot => 
                List[ir.Req]() ++
                annot.happens.map(happensReq) ++
                annot.readable.map(readableReq) ++
                annot.writable.map(writableReq) ++
                annot.subinterval.map(subintervalReq)
        }
    }   
    
    // ___ Dummy Entries ____________________________________________________ 
    //
    // In case of errors when translating something, we insert a dummy
    // entry as an attempt to recover.
    
    def dummyLvDecl(velem: VariableElement) =
        ir.LvDecl(
            lv(velem),
            ir.t_void
        )
    
    def dummyFieldDecl(velem: VariableElement) =
        ir.ReifiedFieldDecl(
            ir.noAttrs,
            ir.t_void,
            f(velem),
            ir.p_this_creator
        )

    def dummyMethodDecl(eelem: ExecutableElement) =
        ir.MethodDecl(
            /* wt_ret: */ ir.t_void,
            /* name:   */ m(eelem), 
            /* args:   */ eelem.getParameters.map(dummyLvDecl).toList,
            /* uncons: */ ir.FullyConstructed,
            /* reqs:   */ List(),
            /* body:   */ ir.empty_method_body
        )

    def dummyClassDecl(telem: TypeElement) = 
        ir.ClassDecl(
            /* Attrs:   */  classAttrs(telem),
            /* Name:    */  className(telem),
            /* TyVars:  */  List(),
            /* TyArgs:  */  List(),
            /* Extends: */  List(),
            /* Ghosts:  */  List(),
            /* Reqs:    */  List(),
            /* Ctor:    */  List(ir.md_ctor_interface),
            /* Fields:  */  List(),
            /* Methods: */  List()
        )    
    
    // ___ Translating the class interface __________________________________
    //
    // The class interface includes all fields, methods, constructors, etc
    // but does not include any method bodies.
    
    def classAttrs(telem: TypeElement) = {
        if(telem.getKind.isInterface) ir.interfaceAttrs
        else ir.noAttrs
    }
    
    def fieldGuard(env: TranslateEnv)(velem: VariableElement) = 
        log.indented("fieldGuard(%s)", velem) {
            at(ElementPosition(velem), ir.p_this_creator) {
                val s_guard = 
                    if(velem.getAnnotation(classOf[WrittenDuring]) != null) {
                        velem.getAnnotation(classOf[WrittenDuring]).value
                    } else if(velem.getAnnotation(classOf[GuardedBy]) != null) {
                        velem.getAnnotation(classOf[GuardedBy]).value
                    } else if(EU.isFinal(velem)) {
                        val telem_owner = EU.enclosingClass(velem)
                        val cn = className(telem_owner)
                        ir.CtorFieldName(Some(cn)).thisPath.toString                        
                    } else
                        ir.f_creator.thisPath.toString
                AnnotParser(env).path(s_guard)
            }            
        }
    
    def intFieldDecl(velem: VariableElement) = 
        indexLog.indented("Field: %s", qualName(velem)) {
            at(ElementPosition(velem), dummyFieldDecl(velem)) {
                val env = elemEnv(velem)
                ir.ReifiedFieldDecl(
                    ir.noAttrs,
                    wtref(env)(getAnnotatedType(velem)),
                    f(velem),  
                    fieldGuard(env)(velem)          
                ).withPos(env.pos)
            }
        }
        
    def intArgDecl(velem: VariableElement) =
        indexLog.indented("Arg: %s", qualName(velem)) {
            at(ElementPosition(velem), dummyLvDecl(velem)) {
                ir.LvDecl(
                    lv(velem),
                    wtref(elemEnv(velem))(getAnnotatedType(velem))
                )
            }
        }
        
    def intMethodDecl(eelem: ExecutableElement) = 
        indexLog.indented("Method Inter: %s()", qualName(eelem)) {
            at(ElementPosition(eelem), dummyMethodDecl(eelem)) {
                val elem_owner = EU.enclosingClass(eelem)
                val env_mthd = elemEnv(eelem)
                val annty = getAnnotatedType(eelem)
                val am = annotationMirrorOfElement(eelem, wke.Unconstructed.ty)
                val uncons = unconstructed(className(elem_owner), am)
                ir.MethodDecl(
                    /* wt_ret: */ wtref(env_mthd)(annty.getReturnType), 
                    /* name:   */ m(eelem), 
                    /* args:   */ eelem.getParameters.map(intArgDecl).toList,
                    /* uncons: */ uncons,
                    /* reqs:   */ reqs(eelem),
                    /* body:   */ ir.empty_method_body
                ).withPos(env_mthd.pos)
            }
        }
    
    def intClassDecl(filter: (Element => Boolean), telem: TypeElement): ir.ClassDecl = 
        indexLog.indented("Class Inter: %s", qualName(telem)) {
            at(ElementPosition(telem), dummyClassDecl(telem)) {
                val enclElems = telem.getEnclosedElements
                val ctorDecls = EF.constructorsIn(enclElems).filter(filter).map(intMethodDecl)
                val methodDecls = EF.methodsIn(enclElems).filter(filter).map(intMethodDecl)
                val fieldDecls = EF.fieldsIn(enclElems).filter(filter).map(intFieldDecl)
                
                val env = elemEnv(telem)
                val ghostDecls = ghostFieldsDeclaredOnElem(telem).map { case (f, annty) =>
                    ir.GhostFieldDecl(wtref(env)(annty), f) }        
                val ghosts = ghostFieldsBoundOnElem(telem).map { case (f, s) =>
                    ir.Ghost(f, AnnotParser(env).path(s)) }
        
                // XXX Type Vars, Type Args
                
                ir.ClassDecl(
                    /* Attrs:   */  classAttrs(telem),
                    /* Name:    */  className(telem),
                    /* T Vars:  */  List(),
                    /* T Args:  */  List(),
                    /* Extends: */  directSupertys(telem).map(erasedTy(_)),
                    /* Ghosts:  */  ghosts.toList,
                    /* Reqs:    */  reqs(telem),
                    /* Ctor:    */  ctorDecls.toList,
                    /* Fields:  */  (ghostDecls ++ fieldDecls).toList,
                    /* Methods: */  methodDecls.toList
                ).withPos(env.pos)
            }            
        }
        
    // ___ Translating the class implementation _____________________________
    //
    // The class implementation includes all the classes method bodies.
    
    def implMethodDecl(ek: ElementKind)(tree: Tree, mdecls: List[ir.MethodDecl]): List[ir.MethodDecl] = tree match {
        case mtree: MethodTree =>
            val eelem = TU.elementFromDeclaration(mtree)
            if(eelem.getKind == ek) {
                indexLog.indented("Method Impl: %s()", qualName(eelem)) {
                    at(ElementPosition(eelem), dummyMethodDecl(eelem) :: mdecls) {
                        val intMdecl = intMethodDecl(eelem)
                        val blocks = TranslateMethodBody(logStack, this, mtree)

                        ir.MethodDecl(
                            intMdecl.wt_ret,
                            intMdecl.name,
                            intMdecl.args,
                            intMdecl.unconstructed,
                            intMdecl.reqs,
                            blocks
                        ).withPos(TreePosition(mtree, (s => s))) :: mdecls
                    }
                }                
            } else
                mdecls
        case _ => mdecls
    }
     
    def implClassDecl(ctree: ClassTree): ir.ClassDecl = {
        val telem = TU.elementFromDeclaration(ctree)
        indexLog.indented("Class Impl %s", qualName(telem)) {
            at(ElementPosition(telem), dummyClassDecl(telem)) {
                // Fields are the same for the interface and the implementation:
                val intCdecl = intClassDecl((_ => true), telem)
                val enclElems = telem.getEnclosedElements
                val members = ctree.getMembers.toList
                val ctorDecls = members.foldRight(List[ir.MethodDecl]())(implMethodDecl(EK.CONSTRUCTOR))
                val methodDecls = members.foldRight(List[ir.MethodDecl]())(implMethodDecl(EK.METHOD))
                
                ir.ClassDecl(
                    intCdecl.attrs,
                    intCdecl.name,
                    intCdecl.typeVars,
                    intCdecl.typeArgs,
                    intCdecl.superClasses,
                    intCdecl.ghosts,
                    intCdecl.reqs,
                    ctorDecls,
                    intCdecl.fields,
                    methodDecls
                ).withPos(TreePosition(ctree, (s => s)))
            }
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
            superAtms.foreach(postDirectSuperType)
        }
    }
    
    override def annotateImplicit(elem: Element, atm: AnnotatedTypeMirror) {
        log.indented("annotateImplicit(%s,%s)", elem, atm) {
            elem.getKind match {
                case EK.CLASS | EK.INTERFACE | EK.ENUM | EK.ANNOTATION_TYPE =>
                    atm.clearAnnotations
                case _ =>
            }            
            log("Result: %s", atm)
        }
    }

    override def annotateImplicit(tree: Tree, atm: AnnotatedTypeMirror) {
        tree.getKind match {
            case TRK.CLASS =>
                val elem = TU.elementFromDeclaration(tree.asInstanceOf[ClassTree])
                annotateImplicit(elem, atm)
            case _ =>
        }
    }       
}
