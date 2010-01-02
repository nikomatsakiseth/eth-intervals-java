package checkers.lock;

import java.util.*;

import javax.lang.model.element.AnnotationMirror;

import com.sun.source.tree.*;

import checkers.lock.quals.GuardedBy;
import checkers.types.AnnotatedTypeMirror;
import checkers.types.BasicAnnotatedTypeFactory;
import checkers.util.AnnotationUtils;
import checkers.util.TreeUtils;
import checkers.util.TypesUtils;
import checkers.util.AnnotationUtils.AnnotationBuilder;
import static checkers.util.AnnotationUtils.elementValue;;

// Disclaimer:
// This class is currently in its alpha form.  For sample code on how to write
// checkers, please review other checkers for code samples.

/**
 * The type factory for {@code Lock} type system.
 *
 * The annotated types returned by class contain {@code GuardedBy} type
 * qualifiers only for the locks that are not currently held.
 *
 */
public class LockAnnotatedTypeFactory
    extends BasicAnnotatedTypeFactory<LockChecker> {

    private List<String> heldLocks = new ArrayList<String>();
    private final AnnotationMirror GUARDED_BY;

    public LockAnnotatedTypeFactory(LockChecker checker,
            CompilationUnitTree root) {
        super(checker, root);
        GUARDED_BY = annotations.fromClass(GuardedBy.class);
    }

    public void setHeldLocks(List<String> heldLocks) {
        this.heldLocks = heldLocks;
    }

    public List<String> getHeldLock() {
        return Collections.unmodifiableList(heldLocks);
    }

    private void removeHeldLocks(AnnotatedTypeMirror type) {
        Set<AnnotationMirror> annos = type.getAnnotations();
        if (annos.isEmpty())
            return;
        AnnotationMirror guarded = annos.iterator().next();
        String lock = elementValue(guarded, "value", String.class);
        if (heldLocks.contains(lock))
            type.clearAnnotations();
    }

    private AnnotationMirror createGuarded(String lock) {
        AnnotationUtils.AnnotationBuilder builder =
            new AnnotationUtils.AnnotationBuilder(env, GuardedBy.class.getCanonicalName());
        builder.setValue("value", lock);
        return builder.build();
    }

    private ExpressionTree receiver(ExpressionTree expr) {
        if (expr.getKind() == Tree.Kind.METHOD_INVOCATION)
            expr = ((MethodInvocationTree)expr).getMethodSelect();
        expr = TreeUtils.skipParens(expr);
        if (expr.getKind() == Tree.Kind.MEMBER_SELECT)
            return ((MemberSelectTree)expr).getExpression();
        else
            return null;
    }

    private void replaceThis(AnnotatedTypeMirror type, Tree tree) {
        if (tree.getKind() != Tree.Kind.IDENTIFIER
            && tree.getKind() != Tree.Kind.MEMBER_SELECT
            && tree.getKind() != Tree.Kind.METHOD_INVOCATION)
            return;
        ExpressionTree expr = (ExpressionTree)tree;

        if (!type.hasAnnotation(GUARDED_BY) || TreeUtils.isSelfAccess(expr))
            return;

        AnnotationMirror guardedBy = type.getAnnotation(GuardedBy.class.getCanonicalName());
        if (!"this".equals(elementValue(guardedBy, "value", String.class)))
            return;
        ExpressionTree receiver = receiver(expr);
        assert receiver != null;
        if (receiver != null) {
            AnnotationMirror newAnno = createGuarded(receiver.toString());
            type.clearAnnotations();
            type.addAnnotation(newAnno);
        }
    }

    private void replaceItself(AnnotatedTypeMirror type, Tree tree) {
        if (tree.getKind() != Tree.Kind.IDENTIFIER
            && tree.getKind() != Tree.Kind.MEMBER_SELECT
            && tree.getKind() != Tree.Kind.METHOD_INVOCATION)
            return;
        ExpressionTree expr = (ExpressionTree)tree;

        if (!type.hasAnnotation(GUARDED_BY))
            return;

        AnnotationMirror guardedBy = type.getAnnotation(GuardedBy.class.getCanonicalName());
        if (!"itself".equals(elementValue(guardedBy, "value", String.class)))
            return;

        AnnotationMirror newAnno = createGuarded(expr.toString());
        type.clearAnnotations();
        type.addAnnotation(newAnno);
    }

    @Override
    protected void annotateImplicit(Tree tree, AnnotatedTypeMirror type) {
        super.annotateImplicit(tree, type);
        replaceThis(type, tree);
        replaceItself(type, tree);
        removeHeldLocks(type);
    }

    @Override
    protected AnnotationMirror aliasedAnnotation(AnnotationMirror a) {
        if (TypesUtils.isDeclaredOfName(a.getAnnotationType(),
                net.jcip.annotations.GuardedBy.class.getCanonicalName())) {
            AnnotationBuilder builder = new AnnotationBuilder(env, GuardedBy.class);
            builder.setValue("value", AnnotationUtils.parseStringValue(a, "value"));
            return builder.build();
        } else {
            return super.aliasedAnnotation(a);
        }
    }
}
