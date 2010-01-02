package checkers.javari;

import java.util.*;

import javax.lang.model.element.*;
import javax.lang.model.type.*;

import com.sun.source.tree.*;
import com.sun.source.util.SimpleTreeVisitor;

import checkers.javari.quals.*;
import checkers.util.*;
import checkers.types.*;
import checkers.types.visitors.AnnotatedTypeScanner;
import checkers.types.visitors.SimpleAnnotatedTypeScanner;

import static checkers.types.AnnotatedTypeMirror.*;


/**
 * Generates a AnnotatedTypeMirror with Javari annotations from a Tree
 * or a Element parameter.
 * <p>
 * Implicit annotations are added as follows:
 * <ol>
 *
 *   <li value="1">Qualified class types without annotations receive the
 *   {@code @Mutable} annotation.
 *
 *   <li value="2">Qualified executable types receivers without annotations
 *   are annotated with the qualified executable type owner's
 *   annotation.
 *
 *   <li value="3">Qualified declared types are annotated with their
 *   underlying type's element annotations.
 *
 *   <li value="4">Qualified types whose elements correspond to fields, and
 *   all its subtypes, are annotated with {@code @ReadOnly},
 *   {@code @Mutable} or {@code @PolyRead}, according to the qualified type
 *   of {@code this}.
 *
 *</ol>
 */
public class JavariAnnotatedTypeFactory extends AnnotatedTypeFactory {

    /** Adds annotations from tree context before type resolution. */
    private final JavariTreePreAnnotator treePre;

    /** Adds annotations from the resulting type after type resolution. */
    private final JavariTypePostAnnotator typePost;

    /** The Javari annotations. */
    private final AnnotationMirror READONLY, THISMUTABLE, MUTABLE, POLYREAD, QREADONLY;

    /**
     * Creates a new {@link JavariAnnotatedTypeFactory} that operates on a
     * particular AST.
     *
     * @param checker the checker to which this factory belongs
     * @param root the AST on which this type factory operates
     */
    public JavariAnnotatedTypeFactory(JavariChecker checker,
        CompilationUnitTree root) {
        super(checker, root);
        this.treePre = new JavariTreePreAnnotator();
        this.typePost = new JavariTypePostAnnotator();
        this.READONLY = checker.READONLY;
        this.THISMUTABLE = checker.THISMUTABLE;
        this.MUTABLE = checker.MUTABLE;
        this.POLYREAD = checker.POLYREAD;
        this.QREADONLY = checker.QREADONLY;
    }

    /**
     * Returns the annotation specifying the immutability type of {@code type}.
     */
    private AnnotationMirror getImmutabilityAnnotation(/*@ReadOnly*/ AnnotatedTypeMirror type) {
       if (!type.isAnnotated())
            return null;
       return type.getAnnotations().iterator().next();
    }

    /**
     * @param type  an annotated type mirror
     * @return true iff the type is specified an immutability type
     * other than this-mutable, false otherwise
     */
    public boolean hasImmutabilityAnnotation(/*@ReadOnly*/ AnnotatedTypeMirror type) {
        return type != null && getImmutabilityAnnotation(type) != null;
    }


    /**
     * Adds implicit annotations to a qualified type, based on its
     * tree, as follows:
     *
     * <ul>
     *
     *   <li> 1. Resolves qualified types from MemberSelectTree,
     *   inheritting from the expression to the identifier if the
     *   identifier is {@code @ThisMutable}.
     *
     *   <li> 2. Qualified class types without annotations receive the
     *   {@code @Mutable} annotation.
     *
     *   <li> 3. Qualified executable types receivers without
     *   annotations are annotated with the qualified executable type
     *   owner's annotation.
     *
     *   <li> 4. Qualified executable types parameters and return
     *   values without annotations are annotated with {@code @Mutable}.
     *
     *   <li> 5. Qualified declared types are annotated with their
     *   underlying type's element annotations.
     *
     *   <li> 6. Qualified types whose elements correspond to fields,
     *   and all its subtypes, are annotated with {@code @ReadOnly},
     *   {@code @Mutable} or {@code @PolyRead}, according to the
     *   qualified type of {@code this}.
     *
     * </ul>
     *
     * @param tree an AST node
     * @param type the type obtained from {@code tree}
     */
    @Override
    protected void annotateImplicit(Tree tree, /*@Mutable*/ AnnotatedTypeMirror type) {

        // primitives are all the same
        if (type.getKind().isPrimitive()
            && !hasImmutabilityAnnotation(type)) {
            type.addAnnotation(MUTABLE);
            return;
        }

        // 1 and 2
        treePre.visit(tree, type);

        // 3, 4 and 5
        typePost.visit(type);

        // 6 - resolve ThisMutable from fields
        if (type.hasAnnotation(THISMUTABLE)) {
            AnnotatedDeclaredType selfType = getSelfType(tree);
            if (selfType != null) {
                if (selfType.hasAnnotation(POLYREAD))
                    new AnnotatedTypeReplacer(THISMUTABLE, POLYREAD).visit(type);

                else if (selfType.hasAnnotation(MUTABLE))
                    new AnnotatedTypeReplacer(THISMUTABLE, MUTABLE).visit(type);
            }
        }
    }

    /**
     * Convenience method for annotating two corresponding iterables.
     * Both arguments must iterate through the same number of elements.
     */
    protected void annotateImplicit(Iterable<? extends Tree> trees,
                                    Iterable<? extends AnnotatedTypeMirror> types) {
        Iterator<? extends Tree> iTree = trees.iterator();
        Iterator<? extends AnnotatedTypeMirror> iType = types.iterator();
        while (iTree.hasNext()) {
            assert iType.hasNext();
            annotateImplicit(iTree.next(), iType.next());
        }
        assert !iType.hasNext();
    }


    /**
     * Adds annotations to qualified types according to their provided
     * element, as follows:
     *
     * <ul>
     *
     *   <li> 1. Qualified class types without annotations
     *   corresponding to class or interface elements receive the
     *   {@code @Mutable} annotation.
     *
     *   <li> 2. Unannotated receivers of qualified executable types
     *   are annotated with the qualified type owner's annotation.
     *
     *   <li> 3. Unannotated qualified declared types are annotated
     *   with their underlying type's element annotations.
     *
     *   <li> 4. Qualified types whose elements correspond to fields,
     *   and all its subtypes, are annotated with {@code @ReadOnly} or
     *   {@code @ThisMutable}, according to the supertype.
     *
     * </ul>
     *
     * @param element an element
     * @param type the type obtained from {@code elt}
     */
    @Override
    protected void annotateImplicit(Element element, /*@Mutable*/ AnnotatedTypeMirror type) {
        if (element.getKind().isClass() || element.getKind().isInterface()) {
            if (!hasImmutabilityAnnotation(type))
                type.addAnnotation(MUTABLE);
        }
        typePost.visit(type);
    }

    protected void postDirectSuperTypes(AnnotatedTypeMirror type,
            List<? extends AnnotatedTypeMirror> supertypes) {
        super.postDirectSuperTypes(type, supertypes);
        for (AnnotatedTypeMirror supertype : supertypes)
            typePost.visit(supertype);
    }

    /**
     * Returns a singleton collection with the most restrictive immutability
     * annotation that is a supertype of the annotations on both collections.
     */
    @Override
    public Collection<AnnotationMirror> unify(Collection<AnnotationMirror> c1,
            Collection<AnnotationMirror> c2) {
        Map<String, AnnotationMirror> ann =
            new HashMap<String, AnnotationMirror>();
        for (AnnotationMirror anno : c1)
            ann.put(AnnotationUtils.annotationName(anno).toString(), anno);
        for (AnnotationMirror anno : c2)
            ann.put(AnnotationUtils.annotationName(anno).toString(), anno);

        if (ann.containsKey(QReadOnly.class.getCanonicalName()))
            return Collections.singleton(QREADONLY);
        else if (ann.containsKey(ReadOnly.class.getCanonicalName()))
            return Collections.singleton(READONLY);
        else if (ann.containsKey(PolyRead.class.getCanonicalName()))
            return Collections.singleton(POLYREAD);
        else
            return Collections.singleton(MUTABLE);
    }

    /**
     * Determines the type of the constructed object based on the
     * parameters passed to the constructor. The new object has the
     * same mutability as the annotation marked on the constructor
     * receiver, as resolved by this method.
     *
     * {@code @PolyRead} receiver values are resolved by looking at
     * the mutability of any parameters marked as {@code @PolyRead}. The rules
     * are similar to the ones applicable to
     * method invocation resolution, but without looking at {@code this}.
     *
     * <ul>
     *
     *  <li> 1. If all parameters marked as {@code @PolyRead} receive
     *  {@code @Mutable} arguments, the receiver value is resolved as
     *  {@code @Mutable}.
     *
     *  <li> 2. If all parameters marked as {@code @PolyRead} receive
     *  {@code @Mutable} or {@code @ThisMutable} arguments and the
     *  condition above is not satisfied, the receiver value is
     *  resolved as {@code @ThisMutable}.
     *
     *  <li> 3. If all parameters marked as {@code @PolyRead} receive
     *  {@code @Mutable} or {@code @PolyRead} arguments and none of
     *  the condition above is satisfied, the receiver value is
     *  resolved as {@code @PolyRead}.
     *
     *  <li> 4. If none of the conditions above is satisfied, the
     *  receiver value is resolved as {@code @ReadOnly}.
     *
     * </ul>
     *
     * @param tree  the new class tree
     * @return AnnotatedExecutableType corresponding to the type being
     * constructed, with the resolved type on its receiver.
     */
    @Override
    public AnnotatedExecutableType constructorFromUse(NewClassTree tree) {

        AnnotatedExecutableType exType = super.constructorFromUse(tree);

        List<AnnotatedTypeMirror> argumentTypes = atypes.getAnnotatedTypes(tree.getArguments());
        List<AnnotatedTypeMirror> parameterTypes = atypes.expandVarArgs(exType, tree.getArguments());

        boolean allMutable = true, allPolyRead = true, allThisMutable = true;

        // look at parameters and arguments
        for (int i = 0; i < parameterTypes.size(); i++) {
            AnnotatedTypeMirror pType = parameterTypes.get(i);

            if (pType.hasAnnotation(POLYREAD)) {
                AnnotatedTypeMirror aType = argumentTypes.get(i);

                if (aType.hasAnnotation(THISMUTABLE) || aType.hasAnnotation(POLYREAD))
                    allMutable = false;

                if (aType.hasAnnotation(READONLY) || aType.hasAnnotation(QREADONLY)) {
                    allMutable = false; allThisMutable = false;
                }

                if (!(aType.hasAnnotation(POLYREAD)
                      && !aType.hasAnnotation(READONLY)
                      && !aType.hasAnnotation(THISMUTABLE)
                      && !aType.hasAnnotation(QREADONLY)))
                    allPolyRead = false;
            }
        }

        // replacement: annotation to be put in place of @PolyRead
        AnnotationMirror replacement;
        if (allMutable) replacement = MUTABLE;               // case 1
        else if (allThisMutable) replacement = THISMUTABLE;  // case 2
        else if (allPolyRead) replacement = POLYREAD;        // case 3
        else replacement = READONLY;                         // case 4

        if (replacement != POLYREAD)  // do not replace if replacement is also @PolyRead
            new AnnotatedTypeReplacer(POLYREAD, replacement).visit(exType);

        return exType;
    }

    /**
     * Determines the type of the invoked method based on the passed method
     * invocation tree.
     *
     * Invokes the super method, then resolves annotations {@code @PolyRead}
     * at the raw level on return values by looking at the
     * mutability of any parameters marked as {@code @PolyRead}. For
     * this purpose, a {@code @PolyRead} annotation on the receiver
     * counts as if {@code this} were being passed as an argument to a
     * parameter marked as {@code @PolyRead}.
     *
     * <ul>
     *
     *  <li> 1. If all parameters marked as {@code @PolyRead} receive
     *  {@code @Mutable} arguments, the return value is resolved as
     *  {@code @Mutable}.
     *
     *  <li> 2. If all parameters marked as {@code @PolyRead} receive
     *  {@code @Mutable} or {@code @ThisMutable} arguments and the
     *  condition above is not satisfied, the return value is resolved
     *  as {@code @ThisMutable}.
     *
     *  <li> 3. If all parameters marked as {@code @PolyRead} receive
     *  {@code @Mutable} or {@code @PolyRead} arguments and none of
     *  the condition above is satisfied, the return value is resolved
     *  as {@code @PolyRead}.
     *
     *  <li> 4. If none of the conditions above is satisfied, the
     *  return value is resolved as {@code @ReadOnly}.
     *
     * </ul>
     *
     * @param tree  the method invocation tree
     * @return AnnotatedExecutableType with return value resolved as described.
     */
    @Override
    public AnnotatedExecutableType methodFromUse(MethodInvocationTree tree) {
        AnnotatedExecutableType type = super.methodFromUse(tree);

        ExecutableElement executableElt = type.getElement();
        AnnotatedTypeMirror returnType = type.getReturnType();

        List<AnnotatedTypeMirror> argumentTypes = atypes.getAnnotatedTypes(tree.getArguments());
        List<AnnotatedTypeMirror> parameterTypes = atypes.expandVarArgs(type, tree.getArguments());
        AnnotatedTypeMirror receiverType = type.getReceiverType();

        boolean allMutable = true, allPolyRead = true, allThisMutable = true;

        // look at parameters and arguments
        // TODO: get method properly
        for (int i = 0; i < parameterTypes.size(); i++) {
            AnnotatedTypeMirror pType = parameterTypes.get(i);

            // look at it if parameter is PolyRead
            if (pType.hasAnnotation(POLYREAD)) {
                AnnotatedTypeMirror aType = argumentTypes.get(i);

                if (aType.hasAnnotation(THISMUTABLE) || aType.hasAnnotation(POLYREAD))
                    allMutable = false;

                if (aType.hasAnnotation(READONLY) || aType.hasAnnotation(QREADONLY)) {
                    allMutable = false; allThisMutable = false;
                }

                if (!(aType.hasAnnotation(POLYREAD)
                      && !aType.hasAnnotation(READONLY)
                      && !aType.hasAnnotation(THISMUTABLE)
                      && !aType.hasAnnotation(QREADONLY)))
                    allPolyRead = false;
            }
        }

        // look at receiver type and reference
        if (receiverType.hasAnnotation(POLYREAD)) {
            // if MemberSelectTree, we can just look at the expression tree
            ExpressionTree exprTree = tree.getMethodSelect();
            if (exprTree.getKind() == Tree.Kind.MEMBER_SELECT) {
                MemberSelectTree mst = (MemberSelectTree) exprTree;
                AnnotatedTypeMirror referenceType = getAnnotatedType(mst.getExpression());

                if (referenceType.hasAnnotation(READONLY)) {
                    allMutable = false;
                    allThisMutable = false;
                    allPolyRead = false;
                } else if (referenceType.hasAnnotation(POLYREAD)) {
                    allMutable = false;
                }

            } else {
                // not MemberSelectTree, get context from method's enclosing class
                AnnotatedTypeMirror classType
                    = fromElement(executableElt.getEnclosingElement());
                if (classType.hasAnnotation(READONLY)) {
                    allMutable = false;
                    allThisMutable = false;
                    allPolyRead = false;
                } else if (classType.hasAnnotation(POLYREAD)) {
                    allMutable = false;
                    allThisMutable = false;
                }
            }
        }

        // replacement: annotation to be put in place of @PolyRead
        AnnotationMirror replacement;
        if (allMutable) replacement = MUTABLE;               // case 1
        else if (allThisMutable) replacement = THISMUTABLE;  // case 2
        else if (allPolyRead) replacement = POLYREAD;        // case 3
        else replacement = READONLY;                         // case 4

        if (replacement != POLYREAD && returnType.hasAnnotation(POLYREAD))
            new AnnotatedTypeReplacer(POLYREAD, replacement).visit(type);

        return type;
    }

    /**
     * We modify this callback method to replace {@code @ThisMutable}
     * implicit annotations with the qualified supertype annotation,
     * if the owner doesn't have a {@code @ReadOnly} annotation.
     * <p>
     * Note on the given example that, if {@code @ThisMutable tmObject}
     * were resolved as {@code @ReadOnly tmObject}, the code snippet
     * would be legal. Such a class could then be created to obtain
     * {@code @Mutable} access to {@code tmObject} from a {@code @ReadOnly}
     * reference to it, without typechecker errors.
     *
     * <pre>@PolyRead Object breakJavari(@PolyRead Object s) @ReadOnly {
     *   tmObject = s;
     *   return null;
     *  }
     * </pre>
     *
     * @param type  the annotated type of the element
     * @param owner the annotated type of the receiver of the accessing tree
     * @param element   the element of the field or method
     */
    @Override
    public void postAsMemberOf(AnnotatedTypeMirror type,
            AnnotatedTypeMirror owner, Element element) {
        if (!owner.hasAnnotation(READONLY)) {
            final AnnotationMirror ownerAnno = getImmutabilityAnnotation(owner);
            if (ownerAnno != THISMUTABLE)
                new AnnotatedTypeReplacer(THISMUTABLE, ownerAnno).visit(type);
        }
    }


    /**
     * A visitor to get annotations from a tree.
     *
     * Annotations obtained from a MemberSelectTree are added to the parameter p.
     */
    private class JavariTreePreAnnotator extends SimpleTreeVisitor<Void, AnnotatedTypeMirror> {

        /**
         * Selects the appropriate annotation from the identifier, or
         * inherits it from the expression, as follows:
         *
         * <ul>
         *
         *  <li> 1. If the identifier qualified type is annotated with
         *  {@code @ReadOnly} or {@code @Mutable}, the parameter
         *  receives the same annotation.
         *
         *  <li> 2. If not, and if the expression qualified type has
         *  any annotation, the parameter receives those annotations.
         *
         *  <li> 3. If the expression qualified type has no
         *  annotation, then the parameter receives a {@code
         *  @ThisMutable} annotation.
         *
         * </ul>
         *
         * @param node MemberSelectTree to be analyzed, of the form {@code expression . identifier}
         * @param p AnnotatedTypeMirror parameter to be annotated
         */
        @Override
        public Void visitMemberSelect(MemberSelectTree node, AnnotatedTypeMirror p) {

            AnnotatedTypeMirror exType = getAnnotatedType(node.getExpression());
            AnnotatedTypeMirror idType = fromElement(TreeUtils.elementFromUse(node));

            p.removeAnnotations(p.getAnnotations());
            if (idType.hasAnnotation(READONLY))                     // case 1
                p.addAnnotation(READONLY);
            else if (idType.hasAnnotation(MUTABLE))                 // case 1
                p.addAnnotation(MUTABLE);
            else if (hasImmutabilityAnnotation(exType))             // case 2
                p.addAnnotation(getImmutabilityAnnotation(exType));
            else                                                    // case 3
                p.addAnnotation(THISMUTABLE);

            return super.visitMemberSelect(node, p);
        }

        /**
         * Inserts {@code @Mutable} annotations on null literal trees.
         */
        @Override
        public Void visitLiteral(LiteralTree node, AnnotatedTypeMirror p) {
            if (node.getKind() == Tree.Kind.NULL_LITERAL)
                p.addAnnotation(MUTABLE);
            return super.visitLiteral(node, p);
        }

        /**
         * Combines annotations obtained from constructor declaration
         * and local user annotations.
         */
        @Override
        public Void visitNewClass(NewClassTree node, AnnotatedTypeMirror p) {
            assert p.getKind() == TypeKind.DECLARED;
            AnnotatedTypeMirror receiverType =
                ((AnnotatedExecutableType) constructorFromUse(node)).getReceiverType();
            Collection<AnnotationMirror> receiverAnn = receiverType.getAnnotations(),
                pAnn = p.getAnnotations();
            Collection<AnnotationMirror> unified = unify(pAnn, receiverAnn);
            p.removeAnnotations(pAnn);
            p.addAnnotations(unified);

            return super.visitNewClass(node, p);
        }

        @Override
        public Void visitClass(ClassTree node, AnnotatedTypeMirror p) {
            if (!hasImmutabilityAnnotation(p))
                p.addAnnotation(MUTABLE);
            return super.visitClass(node, p);
        }
    }

    /**
     * Scanner responsible for adding implicit annotations to qualified types.
     *
     * <ul>
     *
     *   <li> 1. Annotates unannotated receivers of qualified
     *   executable types with the qualified type owner's annotation;
     *   annotated its parameters with {@code @Mutable}, if they have
     *   no annotation, and annotates its return type with {@code
     *   @Mutable}, if it has no annotation.
     *
     *   <li> 2. Annotates unannotated qualified declared types with
     *   their underlying type's element annotations.
     *
     *   <li> 3. Annotates unnanotated qualified types whose elements
     *   correspond to fields with {@code @ThisMutable}.
     *
     *   <li> 4. Annotated unnanotated qualified types corresponding to arrays
     *   with {@code @Mutable}.
     *
     * </ul>
     */
    private class JavariTypePostAnnotator extends AnnotatedTypeScanner<Void, Void> {

        /**
         * Annotates scanned qualified types that correspond to fields
         * and have no immutability annotations with {@code @ThisMutable}
         */
        @Override
        public Void scan(AnnotatedTypeMirror type, Void p) {       // case 3
            if (type != null && type.getElement() != null
                && !hasImmutabilityAnnotation(type)
                && type.getElement().getKind().isField())
                type.addAnnotation(THISMUTABLE);

            return super.scan(type, p);
        }

        /**
         * If the receiver has no annotation, annotates it with the
         * same annotation as the executable type owner. If the
         * parameters or the return type have no annotations, annotate
         * it with {@code @Mutable}.
         */
        @Override
        public Void visitExecutable(AnnotatedExecutableType type, Void p) {
            AnnotatedDeclaredType receiver = type.getReceiverType();
            if (!hasImmutabilityAnnotation(receiver)) {                // case 1
                AnnotatedDeclaredType owner = (AnnotatedDeclaredType)
                    getAnnotatedType(type.getElement().getEnclosingElement());
                assert hasImmutabilityAnnotation(owner);
                receiver.addAnnotation(getImmutabilityAnnotation(owner));
            }

            for (AnnotatedTypeMirror t : type.getParameterTypes())
                if (!hasImmutabilityAnnotation(t)
                    && !t.getKind().isPrimitive()
                    && t.getKind() != TypeKind.TYPEVAR)
                    t.addAnnotation(MUTABLE);

            AnnotatedTypeMirror returnType = type.getReturnType();
            if (!hasImmutabilityAnnotation(returnType)
                && !returnType.getKind().isPrimitive()
                && returnType.getKind() != TypeKind.VOID
                && returnType.getKind() != TypeKind.TYPEVAR)
                returnType.addAnnotation(MUTABLE);

            return super.visitExecutable(type, p);
        }

        /**
         * If the declared type has no annotation, annotates it with
         * the same annotation as its underlying type's element.
         */
        @Override
        public Void visitDeclared(AnnotatedDeclaredType type, Void p) {
            if (!hasImmutabilityAnnotation(type)) {                  // case 2
                TypeElement tElt = (TypeElement) type.getUnderlyingType().asElement();
                AnnotatedTypeMirror tType = fromElement(tElt);
                if (hasImmutabilityAnnotation(tType))
                    type.addAnnotation(getImmutabilityAnnotation(tType));
                else
                    type.addAnnotation(MUTABLE);
            }
            return super.visitDeclared(type, p);
        }

        /**
         * Ensures that AnnotatedArrayTypes are annotated with {@code
         * @Mutable}, if they have no annotation yet.
         */
        @Override
        public Void visitArray(AnnotatedArrayType type, Void p) {
            if (!hasImmutabilityAnnotation(type)) {                  // case 4
                type.addAnnotation(MUTABLE);
            }
            return super.visitArray(type, p);
        }

        @Override
        public Void visitTypeVariable(AnnotatedTypeVariable type, Void p) {
//            if (!hasImmutabilityAnnotation(type))
//                type.addAnnotation(QREADONLY);
            AnnotatedTypeMirror upperBound = type.getUpperBound();
            if (upperBound != null
                && !hasImmutabilityAnnotation(upperBound)
                && upperBound.toString().equals("Object"))
                upperBound.addAnnotation(READONLY);
            return super.visitTypeVariable(type, p);
        }

        @Override
        public Void visitWildcard(AnnotatedWildcardType type, Void p) {
            if (type.getExtendsBound() != null
                    && !hasImmutabilityAnnotation(type.getExtendsBound()))
                type.getExtendsBound().addAnnotation(READONLY);
            return super.visitWildcard(type, p);
        }
    }


    /**
     * Type scanner to replace annotations on annotated types.
     */
    private class AnnotatedTypeReplacer extends SimpleAnnotatedTypeScanner<Void, Void> {
        private AnnotationMirror oldAnnotation, newAnnotation;

        /** Initializes the class to replace oldAnnotation with newAnnotation. */
        AnnotatedTypeReplacer(AnnotationMirror oldAnnotation, AnnotationMirror newAnnotation) {
            this.oldAnnotation = oldAnnotation;
            this.newAnnotation = newAnnotation;
        }

        @Override
        protected Void defaultAction(AnnotatedTypeMirror type, Void p) {
            if (type.hasAnnotation(oldAnnotation)) {
                type.removeAnnotation(oldAnnotation);
                type.addAnnotation(newAnnotation);
            }
            return super.defaultAction(type, p);
        }
    }
}
