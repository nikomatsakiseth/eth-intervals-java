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

    // ___ Parsing Annotations ______________________________________________
    
    def findAm(ams: Iterable[AnnotationMirror], ty: TypeMirror): Option[AnnotationMirror] = {
        ams.find(am => types.isSameType(am.getAnnotationType, ty))
    }
    
    /** Extracts the `value()` argument from `am` as a String. */
    def annValue(am: AnnotationMirror): String = 
        AU.parseStringValue(am, "value")

    /** If `elem` is annotated with `@DefinesGhost(classOf=C, useByDefault=B)`, returns `(C,B)`. */
    def definedGhost(elem: Element): Option[(TypeMirror, Boolean)] = {
        log.indented(false, "definedGhost(%s)", elem) {
            findAm(elem.getAnnotationMirrors, wke.DefinesGhost.ty).map { am =>
                val evalues = elements.getElementValuesWithDefaults(am)
                val ofClass = evalues.get(wke.ofClass).getValue.asInstanceOf[TypeMirror]
                val useByDefault = evalues.get(wke.useByDefault).getValue.asInstanceOf[java.lang.Boolean]
                (ofClass, useByDefault.booleanValue)
            }
        }
    }

    // ___ Misc. Helpers ____________________________________________________
    
    /** Returns `elem` if it is a class, otherwise the class enclosing 'elem' (or null) */
    def closestClass(elem: Element): TypeElement = EU.enclosingClass(elem)

    // Returns 'elem' if it is a package, otherwise the package enclosing 'elem'
    def closestPackage(elem: Element): PackageElement = elem.getKind match {
        case EK.PACKAGE => elem.asInstanceOf[PackageElement]
        case _ => closestPackage(elem.getEnclosingElement)
    }
        
    // Converts from a type element to an ir.ClassName
    def className(typeElem: TypeElement) = {
        ir.ClassName(qualName(typeElem))        
    }
        
    def typeVarName(tpElem: TypeParameterElement) = {
        ir.TypeVarName("%s.%s".format(
            qualName(tpElem.getGenericElement),
            tpElem.getSimpleName
        ))        
    }

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
        
    def asOptionDeclaredType(ty: TypeMirror): Option[DeclaredType] = 
        ty.getKind match {
            case TK.DECLARED => Some(ty.asInstanceOf[DeclaredType])
            case _ => None
        }
        
    def elemOfType(ty: TypeMirror): Option[TypeElement] =
        asOptionDeclaredType(ty).map(_.asElement.asInstanceOf[TypeElement])
        
    def elemOfAnnty(annty: AnnotatedTypeMirror): Option[TypeElement] =
        elemOfType(annty.getUnderlyingType)
        
    def elemKind(annty: AnnotatedTypeMirror): Option[ElementKind] =
        elemOfAnnty(annty).map(_.getKind)
        
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
    def fieldName(elem: Element): ir.FieldName =
        ir.PlainFieldName(qualName(elem))              
        
    def localVariableName(velem: VariableElement): ir.VarName =
        ir.VarName(velem.getSimpleName.toString)
        
    // Given a field name like "a.b.c", returns "c"
    def shortFieldName(f: ir.FieldName) = f match {
        case ir.PlainFieldName(name) if name.contains(".") =>
            val idx = name.lastIndexOf(".")
            name.substring(idx+1)
        case _ => f.toString
    }
    
    def methodName(name: String, argTypes: List[TypeMirror]): ir.MethodName = {
        val sb = new StringBuilder()
        sb.append(name)
        
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

    def methodName(name: String, etm: ExecutableType): ir.MethodName = {
        methodName(name, etm.getParameterTypes.toList)
    }
    
    def methodName(eelem: ExecutableElement): ir.MethodName = {
        methodName(eelem.getSimpleName.toString, eelem.asType.asInstanceOf[ExecutableType])
    }
    
    def findField(ty_owner: TypeMirror, s: String): Option[(VariableElement, TypeMirror)] = {
        asOptionDeclaredType(ty_owner) match {
            case None => None
            case Some(dty_owner) =>
                val telem_owner = dty_owner.asElement.asInstanceOf[TypeElement]
                val velems_fields = EF.fieldsIn(elements.getAllMembers(telem_owner))
                val nm = elements.getName(s)
                velems_fields.find(_.getSimpleName == nm).map(velem_field =>
                    (velem_field, types.asMemberOf(dty_owner, velem_field)))
        }
    }
    
    def identityAnnot(elem: Element) = {
        Option(elem.getAnnotation(classOf[Is])) match {
            case Some(annot) =>
                val env = elemEnv(elem)
                List(AnnotParser(env).wpath(annot.value))
            case None =>
                List()
        }
    }
        
    // ___ Ghost Fields _____________________________________________________
    
    sealed abstract class GhostAnn
    case object GhostAnnNone extends GhostAnn
    sealed case class GhostAnnDecl(f: ir.FieldName, ty: TypeMirror, byDefault: Boolean) extends GhostAnn
    sealed case class GhostAnnValue(f: ir.FieldName, value: String) extends GhostAnn
    
    def categorizeGhostAnnot(am: AnnotationMirror) =
        log.indented("categorizeGhostAnnot(%s)", am) {
            val elem = am.getAnnotationType.asElement
            val value = annValue(am)
            definedGhost(elem) match {
                case None => 
                    GhostAnnNone
                case Some((ty, ubd)) if (value == "") => 
                    GhostAnnDecl(fieldName(elem), ty, ubd)
                case Some(_) => 
                    GhostAnnValue(fieldName(elem), value)                
            }
        }
    
    // Higher-level function that folds elemFunc over
    // every element in 'ty' and its supertypes, progressively
    // adding to the map 'm0' and returning the final result.
    def addTyAndSupertypes[K, V](
        elemFunc: ((Map[K, V], Element) => Map[K, V])
    )(
        m0: Map[K, V],
        ty: TypeMirror
    ): Map[K, V] =
        elemOfType(ty).foldLeft(m0)(addElemAndSuperelems(elemFunc))
        
    // Higher-level function that folds elemFunc over
    // elem and the elements of its supertypes, progressively
    // adding to the map 'm0' and returning the final result.
    def addElemAndSuperelems[K, V](
        elemFunc: ((Map[K, V], Element) => Map[K, V])
    )(
        m0: Map[K, V],
        elem: Element
    ): Map[K, V] = {
        elem.getKind match {
            case EK.CLASS | EK.INTERFACE | EK.ENUM | EK.ANNOTATION_TYPE =>
                val telem = elem.asInstanceOf[TypeElement]
                val m1 = directSupertys(telem).foldLeft(m0)(addTyAndSupertypes(elemFunc))
                elemFunc(m1, telem)
                
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
            case (m, GhostAnnDecl(_, _, _)) => m
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
        m0: Map[ir.FieldName, TypeMirror],
        elem0: Element
    ): Map[ir.FieldName, TypeMirror] = {
        log.indented("addGhostFieldsDeclaredOnElem(%s)", elem0) {
            elem0.getAnnotationMirrors.map(categorizeGhostAnnot).foldLeft(m0) {
                case (m, GhostAnnNone) => m
                case (m, GhostAnnDecl(f, ty, ubd)) => m + (f -> ty)
                case (m, GhostAnnValue(_, _)) => m
            }
        }        
    }
    
    def ghostFieldsDeclaredOnElem(elem: Element) = {
        log.indented("ghostFieldsDeclaredOnElem(%s)", elem) {
            addGhostFieldsDeclaredOnElem(Map.empty, elem)
        }        
    }
                
    def ghostFieldsDeclaredOnElemAndSuperelems(elem: Element) =
        log.indented("ghostFieldsDeclaredOnElemAndSuperelems(%s)", elem) {
            addElemAndSuperelems(addGhostFieldsDeclaredOnElem)(Map.empty, elem)
        }
    
    def ghostFieldsDeclaredOnTyAndSupertypes(ty: TypeMirror) =
        log.indented("ghostFieldsDeclaredOnTyAndSupertypes(%s)", ty) {
            addTyAndSupertypes(addGhostFieldsDeclaredOnElem)(Map.empty, ty)
        }
    
    // ______ Default Ghost Fields __________________________________________
    //
    // By default, ghost fields declared on a class C are included in types
    // that appear in its definition (if relevant).
    
    def addDefaultGhostFieldsDeclaredOnElem(
        m0: Map[ir.FieldName, TypeMirror],
        elem0: Element
    ): Map[ir.FieldName, TypeMirror] =
        log.indented("addGhostFieldsDeclaredOnElem(%s)", elem0) {
            elem0.getAnnotationMirrors.map(categorizeGhostAnnot).foldLeft(m0) {
                case (m, GhostAnnDecl(f, ty, true)) => m + (f -> ty)
                case (m, _) => m
            }
        }
        
    def defaultGhostFieldsDeclaredOnElemAndSuperelems(elem: Element) =
        log.indented("defaultGhostFieldsDeclaredOnElemAndSuperelems(%s)", elem) {
            addElemAndSuperelems(addDefaultGhostFieldsDeclaredOnElem)(Map.empty, elem)
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
        m_localVariables: Map[String, (ir.Path, TypeMirror)],
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
                val dty_this = telem.asType.asInstanceOf[DeclaredType]

                // ----------------------------------------------------------------------
                // "Local variables:"
                //
                // At the class level, paths may begin with "this" or any of the fields, 
                // ghost and reified, declared on the class.
                //
                // TODO-- Detect possible aliases among short field names here and do not
                // add to the dictionary.

                var m_lvs = Map.empty[String, (ir.Path, TypeMirror)]

                val elems_fields = EF.fieldsIn(elements.getAllMembers(telem))
                m_lvs = elems_fields.foldLeft(m_lvs) { case (m, elem) =>
                    val f = fieldName(elem)
                    m + (shortFieldName(f) -> Pair(f.thisPath, types.asMemberOf(dty_this, elem)))
                }

                val ghostFields = ghostFieldsDeclaredOnElemAndSuperelems(telem)
                m_lvs = ghostFields.foldLeft(m_lvs) { case (m, (f, ty)) =>
                    m + (shortFieldName(f) -> Pair(f.thisPath, ty))
                }

                m_lvs += (ir.lv_this.name -> Pair(ir.p_this, dty_this))

                // ----------------------------------------------------------------------
                // Default Ghosts:
                //
                // All in-scope ghost parameters G are automatically supplied to types
                // within (i.e., @G("G") is the default for any in-scope ghost parameter).

                val defaultGhostFields = defaultGhostFieldsDeclaredOnElemAndSuperelems(telem)
                var m_defaultWghosts = Map.empty[ir.FieldName, ir.WcPath]          
                m_defaultWghosts = defaultGhostFields.foldLeft(m_defaultWghosts) { 
                    case (m, (f, _)) => m + (f -> f.thisPath)
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
                // Add final method arguments to the map of things that can start a path.
                // Each arg x -> (p, annty) where p = x and annty = annty(x)

                var m_lvs = env_cls.m_localVariables
                m_lvs += ("method" -> (ir.p_mthd, wke.Interval.ty))
                m_lvs = eelem.getParameters.foldLeft(m_lvs) { case (m, velem) =>
                    if(EU.isFinal(velem)) {
                        val lv = localVariableName(velem)
                        m + (velem.getSimpleName.toString -> (lv.path, velem.asType))                        
                    } else m
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
    
    case class ParsePath(p: ir.Path, ty: TypeMirror)
    
    val parserLog = log // log is inherited from BaseParser, so create an alias
    class AnnotParser(env: TranslateEnv) extends BaseParser {
        def p = (
            c~"#"~f                 ^^ { case c~_~f => ir.VarName(c+"#"+f).path } // XXX Really support static fields
        |   id~rep("."~>f)          ^^ { case s~fs => fs.foldLeft(startPath(s))(extendPath).p }
        |   f~repsep(f, ".")        ^^ { case f~fs => (f :: fs).foldLeft(startPath("this"))(extendPath).p }
        )
        
        def startPath(id: String): ParsePath = {
            parserLog("startPath(%s)", id)
            env.m_localVariables.get(id) match {
                case Some((p, ty)) => ParsePath(p, ty)
                case None => throw new CheckFailure("intervals.no.such.variable", id)
            }
        }
        
        def extendPath(pp: ParsePath, f_ext: ir.FieldName) = {
            parserLog("extendPath(%s, %s)", pp, f_ext)
            val declGhosts = elemOfType(pp.ty).map(ghostFieldsDeclaredOnElemAndSuperelems).getOrElse(Map.empty)

            // note: We use this semi-awkward 'if/else' construction
            // to work around some bug in the scala compiler that caused
            // its code generator to fail.
            
            if(declGhosts.isDefinedAt(f_ext))
                // Exact match:
                ParsePath(pp.p / f_ext, declGhosts(f_ext))
                
            else f_ext match {
                // Built-in constructor intervals:
                case ir.ClassCtorFieldName(_) =>
                    ParsePath(pp.p / f_ext, wke.Interval.ty)                    
                case ir.f_objCtor() =>
                    ParsePath(pp.p / f_ext, wke.Interval.ty)                    
                
                // Search for a non-ambigious short-name match:
                case ir.PlainFieldName(str_ext) =>
                    def hasMatchingShortName(f: ir.FieldName) =
                        shortFieldName(f) == str_ext
                    val potentialMatches = declGhosts.keySet.filter(hasMatchingShortName).toList
                    potentialMatches match {
                        // Single match: use the ghost
                        case List(f_g) => 
                            ParsePath(pp.p / f_g, declGhosts(f_g))
                        
                        // No matchs, search for a real field with that name:
                        case List() => 
                            findField(pp.ty, str_ext) match {
                                case None => throw new CheckFailure("intervals.no.such.field", pp.p, f_ext)
                                case Some((elem_ext, ty_ext)) => 
                                    ParsePath(pp.p / fieldName(elem_ext), ty_ext)
                            }
                            
                        // Multiple matches, error:
                        case fs => 
                            throw new CheckFailure("intervals.ambig.ghost", fs.mkString(", "))
                    }                    
            }
        }
        
        def req(s: String): ir.Req = {
            parserLog.indented("parse req(%s)", s) {
                val req = parseToResult(reqBase)(s)
                req.withPos(env.pos)
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
                
            case TK.WILDCARD | TK.NULL =>
                ir.c_object
            
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
    
    def wtarg(env: TranslateEnv)(
        tv: ir.TypeVarName, 
        annty: AnnotatedTypeMirror
    ): ir.WcTypeArg = {
        log.indented("wtarg(%s, %s)", tv, annty) {
            annty.getKind match {
                case TK.WILDCARD =>
                    def trbnd(bnd: AnnotatedTypeMirror) = 
                        if(bnd == null) List()
                        else List(wtref(env, ir.wgs_constructed)(bnd))
                    val annwty = annty.asInstanceOf[AnnotatedWildcardType]
                    val bnds = ir.TypeBounds(
                        wts_lb = trbnd(annwty.getExtendsBound),
                        wts_ub = trbnd(annwty.getSuperBound)
                    )
                    ir.BoundedTypeArg(tv, bnds)
                    
                case _ => 
                    ir.TypeArg(tv, wtref(env, ir.wgs_constructed)(annty))
            }
        }
    }
    
    def wtref(env: TranslateEnv, wgs_default: List[ir.WcGhost])(
        annty: AnnotatedTypeMirror
    ): ir.WcTypeRef = {
        log.indented("wtref(%s)(%s)", wgs_default, annty) {
            val c = erasedTy(annty.getUnderlyingType)
            
            val wt = annty.getKind match {
                case TK.DECLARED =>
                    val anndty = annty.asInstanceOf[AnnotatedDeclaredType]
                    
                    // Translate type arguments, if any:
                    val wtargs = if(!anndty.isParameterized) {
                        List()
                    } else {
                        val elem = anndty.getUnderlyingType.asElement.asInstanceOf[TypeElement]
                        val tvs = elem.getTypeParameters.map(typeVarName).toList
                        tvs.zip(anndty.getTypeArguments).map { case (tv, annty_arg) =>
                            wtarg(env)(tv, annty_arg) }
                    }
                    
                    ir.WcClassType(c, wghosts(env)(annty), wtargs.toList)

                case TK.ARRAY =>
                    val annaty = annty.asInstanceOf[AnnotatedArrayType]
                    val wt_comp = wtref(env, wgs_default)(annaty.getComponentType)
                    val targ = ir.TypeArg(ir.tv_arrayElem, wt_comp)
                    ir.WcClassType(c, wghosts(env)(annty), List(targ))
                    
                case TK.TYPEVAR =>
                    val anntv = annty.asInstanceOf[AnnotatedTypeVariable]
                    val elem = anntv.getUnderlyingType.asElement 
                    val tv = typeVarName(elem.asInstanceOf[TypeParameterElement])
                    ir.PathType(ir.p_this, tv)

                case _ =>
                    ir.WcClassType(c, List(), List())
            }
            
            wt.withDefaultWghosts(wgs_default)                        
        }
    }

    // ___ Defaults _________________________________________________________
    //
    // Scan for an @Defaults on an element.  If none is found, instantiate
    // the default.  
    
    /** Searches for an annotation of type `tinfo` on `elem0`.  If not found, then searches
      * up all enclosing elements of `elem0`.  If it STILL can't find one, then it just
      * synthesizes one: only legal for use on an annotation where all fields have default
      * values, such as `Defaults` or `BaseRequirements` */
    private[this] def searchForAnnotation(
        tinfo: wke.TypeInfo[_ <: java.lang.annotation.Annotation], 
        elem0: Element
    ) = {
        def search(elem: Element): AnnotationMirror = {
            if(elem == null) new AU.AnnotationBuilder(processingEnvironment, tinfo.cls).build
            else findAm(elem.getAnnotationMirrors, tinfo.ty).getOrElse(search(elem.getEnclosingElement))
        }
        search(elem0)
    }
    
    def defaults(elem: Element): AnnotationMirror = searchForAnnotation(wke.Defaults, elem)
    
    // ___ Requirements _____________________________________________________
    
    def baseRequirementsAm(elem: Element) = searchForAnnotation(wke.BaseRequirements, elem)
    
    def methodReqs(eelem: ExecutableElement): List[ir.Req] = {
        val env = elemEnv(eelem)
        val baseAm = baseRequirementsAm(eelem)
        val baseReqs = eelem.getKind match {
            case EK.CONSTRUCTOR => AU.parseStringArrayValue(baseAm, "constructor").toList
            case EK.METHOD if !EU.isStatic(eelem) => AU.parseStringArrayValue(baseAm, "instanceMethod").toList
            case _ => List()
        }
        val addReqs = findAm(eelem.getAnnotationMirrors, wke.Requires.ty) match {
            case Some(am) => AU.parseStringArrayValue(am, "value").toList
            case None => List()
        }
        (baseReqs ++ addReqs).map(AnnotParser(env).req)
    }   
    
    // ___ Dummy Entries ____________________________________________________ 
    //
    // In case of errors when translating something, we insert a dummy
    // entry as an attempt to recover.
    
    def dummyLvDecl(velem: VariableElement) =
        ir.LvDecl(
            localVariableName(velem),
            ir.t_void,
            List()
        )
    
    def dummyFieldDecl(velem: VariableElement) =
        ir.ReifiedFieldDecl(
            ir.t_void,
            fieldName(velem),
            ir.p_this_creator,
            List()
        )

    def dummyMethodDecl(eelem: ExecutableElement) =
        ir.MethodDecl(
            name = methodName(eelem),
            args = eelem.getParameters.map(dummyLvDecl).toList,
            reqs = List(),
            wt_ret = ir.t_void,
            wps_identity = List(),
            body = ir.empty_method_body
        )

    def dummyClassDecl(telem: TypeElement) = 
        ir.ClassDecl(
            attrs = classAttrs(telem),
            name = className(telem),
            ghostFieldDecls = List(),
            typeVarDecls = List(),
            superClasses = List(ir.c_any),
            ghosts = List(),
            typeArgs = List(),
            reqs = List(),
            ctors = List(ir.md_emptyCtor),
            reifiedFieldDecls = List(),
            methods = List()
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
                    if(velem.getAnnotation(classOf[GuardedBy]) != null) {
                        velem.getAnnotation(classOf[GuardedBy]).value
                    } else if(EU.isFinal(velem)) {
                        val telem_owner = EU.enclosingClass(velem)
                        val cn = className(telem_owner)
                        ir.ClassCtorFieldName(cn).thisPath.toString
                    } else
                        ir.f_creator.thisPath.toString
                AnnotParser(env).path(s_guard)
            }            
        }
    
    def intFieldDecl(velem: VariableElement) = {
        indexLog.indented("Field: %s", qualName(velem)) {
            at(ElementPosition(velem), dummyFieldDecl(velem)) {
                val env = elemEnv(velem)                
                ir.ReifiedFieldDecl(
                    wt           = wtref(env, ir.wgs_fieldsDefault)(getAnnotatedType(velem)),
                    name         = fieldName(velem),  
                    p_guard      = fieldGuard(env)(velem),
                    wps_identity = identityAnnot(velem)
                ).withPos(env.pos)
            }
        }        
    }
        
    def intArgDecl(velem: VariableElement) = {
        indexLog.indented("Arg: %s", qualName(velem)) {
            at(ElementPosition(velem), dummyLvDecl(velem)) {
                ir.LvDecl(
                    name         = localVariableName(velem),
                    wt           = wtref(elemEnv(velem), ir.wgs_constructed)(getAnnotatedType(velem)),
                    wps_identity = identityAnnot(velem)
                )
            }
        }        
    }
        
    def intMethodDecl(eelem: ExecutableElement) = {
        indexLog.indented("Method Inter: %s()", qualName(eelem)) {
            at(ElementPosition(eelem), dummyMethodDecl(eelem)) {
                val elem_owner = EU.enclosingClass(eelem)
                val env_mthd = elemEnv(eelem)
                val annty = getAnnotatedType(eelem)
                ir.MethodDecl(
                    wt_ret       = wtref(env_mthd, ir.wgs_constructed)(annty.getReturnType),
                    wps_identity = identityAnnot(eelem),
                    name         = methodName(eelem), 
                    args         = eelem.getParameters.map(intArgDecl).toList,
                    reqs         = methodReqs(eelem),
                    body         = ir.empty_method_body
                ).withPos(env_mthd.pos)
            }
        }        
    }
        
    def intTypeVarDecl(tvelem: TypeParameterElement): ir.TypeVarDecl = {
        ir.TypeVarDecl(
            name = typeVarName(tvelem),
            wts_lb = List(ir.c_object.ct) /* XXX */
        )
    }
    
    def intClassDecl(filter: (Element => Boolean), telem: TypeElement): ir.ClassDecl = 
        indexLog.indented("Class Inter: %s", qualName(telem)) {
            at(ElementPosition(telem), dummyClassDecl(telem)) {                
                val env = elemEnv(telem)
                
                // Translate the interface of those members accepted by `filter`:
                val enclElems = telem.getEnclosedElements
                val ctorDecls = EF.constructorsIn(enclElems).filter(filter).map(intMethodDecl)
                val methodDecls = EF.methodsIn(enclElems).filter(filter).map(intMethodDecl)
                val fieldDecls = EF.fieldsIn(enclElems).filter(filter).map(intFieldDecl)
                val tvDecls = telem.getTypeParameters.map(intTypeVarDecl)
                
                // Extract bound type arguments from superclass:
                val tas_bound = {
                    def typeArgs(annty: AnnotatedTypeMirror) = {
                        wtref(env, ir.wgs_constructed)(annty) match {
                            case ir.WcClassType(_, _, wtargs) => 
                                // Java does not allow wildcards in supertypes:
                                wtargs.map(_.asInstanceOf[ir.TypeArg])
                            case _ => List()
                        }
                    }
    
                    getAnnotatedType(telem).directSuperTypes.flatMap(typeArgs)
                }
                
                // Extract ghosts declared and bound ghosts from annotations:
                val gfds = ghostFieldsDeclaredOnElem(telem).map { case (f, ty) =>
                    val c = elemOfType(ty).map(className).get
                    ir.GhostFieldDecl(f, c) }
                val gs_bound = ghostFieldsBoundOnElem(telem).map { case (f, s) =>
                    ir.Ghost(f, AnnotParser(env).path(s)) }
        
                ir.ClassDecl(
                    attrs = classAttrs(telem),
                    name = className(telem),
                    ghostFieldDecls = gfds.toList,
                    typeVarDecls = tvDecls.toList,
                    superClasses = directSupertys(telem).map(erasedTy(_)),
                    ghosts = gs_bound.toList,
                    typeArgs = tas_bound.toList,
                    reqs = List(),
                    ctors = ctorDecls.toList,
                    reifiedFieldDecls = fieldDecls.toList,
                    methods = methodDecls.toList
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
                logStack.ifPertinent("TranslateMethodBody(%s)".format(methodName(eelem))) {
                    at(ElementPosition(eelem), dummyMethodDecl(eelem) :: mdecls) {
                        val intMdecl = intMethodDecl(eelem)
                        val body = TranslateMethodBody(logStack, this, mtree)
                        
                        intMdecl.copy(
                            body = body
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
                    attrs = intCdecl.attrs,
                    name = intCdecl.name,
                    typeVarDecls = intCdecl.typeVarDecls,
                    typeArgs = intCdecl.typeArgs,
                    superClasses = intCdecl.superClasses,
                    ghosts = intCdecl.ghosts,
                    ghostFieldDecls = intCdecl.ghostFieldDecls,
                    reqs = intCdecl.reqs,
                    ctors = ctorDecls,
                    reifiedFieldDecls = intCdecl.reifiedFieldDecls,
                    methods = methodDecls
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
