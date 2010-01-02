package tests;

import java.io.File;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.runner.Runner;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.Suite;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.Statement;
import org.junit.runners.model.TestClass;

/**
 * <p>
 * The custom runner <code>Parameterized</code> implements parameterized tests.
 * When running a parameterized test class, instances are created for the
 * cross-product of the test methods and the test data elements.
 * </p>
 *
 * For example, to test a Fibonacci function, write:
 *
 * <pre>
 * &#064;RunWith(Parameterized.class)
 * public class FibonacciTest {
 *  &#064;Parameters
 *  public static Collection&lt;Object[]&gt; data() {
 *      return Arrays.asList(new Object[][] {
 *              Fibonacci,
 *              { { 0, 0 }, { 1, 1 }, { 2, 1 }, { 3, 2 }, { 4, 3 }, { 5, 5 },
 *                      { 6, 8 } } });
 *  }
 *
 *  private int fInput;
 *
 *  private int fExpected;
 *
 *  public FibonacciTest(int input, int expected) {
 *      fInput= input;
 *      fExpected= expected;
 *  }
 *
 *  &#064;Test
 *  public void test(@HeresHowYouGetValue Type value) {
 *      assertAnswerKey(new Object[][] {
 *              Fibonacci,
 *              { { 0, 0 }, { 1, 1 }, { 2, 1 }, { 3, 2 }, { 4, 3 }, { 5, 5 },
 *                      { 6, 8 } } });
 *      assertEquals(fExpected, Fibonacci.compute(fInput));
 *  }
 * }
 * </pre>
 *
 * <p>
 * Each instance of <code>FibonacciTest</code> will be constructed using the
 * two-argument constructor and the data values in the
 * <code>&#064;Parameters</code> method.
 * </p>
 */
public class CheckerParameterized extends Suite {

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    public @interface Name {}

    private class TestClassRunnerForParameters extends
            BlockJUnit4ClassRunner {
        private final int fParameterSetNumber;

        private final List<Object[]> fParameterList;

        TestClassRunnerForParameters(Class<?> type,
                List<Object[]> parameterList, int i) throws InitializationError {
            super(type);
            fParameterList= parameterList;
            fParameterSetNumber= i;
        }

        @Override
        public Object createTest() throws Exception {
            return getTestClass().getOnlyConstructor().newInstance(
                    computeParams());
        }

        private Object[] computeParams() throws Exception {
            try {
                return fParameterList.get(fParameterSetNumber);
            } catch (ClassCastException e) {
                throw new Exception(String.format(
                        "%s.%s() must return a Collection of arrays.",
                        getTestClass().getName(), getParametersMethod(
                                getTestClass()).getName()));
            }
        }

        String testCaseName() {
            File file = (File)fParameterList.get(fParameterSetNumber)[0];
            String name = file.getPath().replace(".java", "").replace("tests/", "");
            if (name.contains("/"))
                return name.substring(name.indexOf("/") + 1);
            else
                return name;
        }

        @Override
        protected String getName() {
            return String.format("[%s]", testCaseName());
        }

        @Override
        protected String testName(final FrameworkMethod method) {
            return String.format("%s[%s]", method.getName(),
                    testCaseName());
        }

        @Override
        protected void validateZeroArgConstructor(List<Throwable> errors) {
            // constructor can, nay, should have args.
        }

        @Override
        protected Statement classBlock(RunNotifier notifier) {
            return childrenInvoker(notifier);
        }
    }

    private final ArrayList<Runner> runners= new ArrayList<Runner>();

    /**
     * Only called reflectively. Do not use programmatically.
     */
    public CheckerParameterized(Class<?> klass) throws Throwable {
        super(klass, Collections.<Runner>emptyList());
        List<Object[]> parametersList= getParametersList(getTestClass());
        for (int i= 0; i < parametersList.size(); i++)
            runners.add(new TestClassRunnerForParameters(getTestClass().getJavaClass(),
                    parametersList, i));
    }

    @Override
    protected List<Runner> getChildren() {
        return runners;
    }

    @SuppressWarnings("unchecked")
    private List<Object[]> getParametersList(TestClass klass)
            throws Throwable {
        return (List<Object[]>) getParametersMethod(klass).invokeExplosively(
                null);
    }

    private FrameworkMethod getParametersMethod(TestClass testClass)
            throws Exception {
        List<FrameworkMethod> methods= testClass
                .getAnnotatedMethods(Parameters.class);
        for (FrameworkMethod each : methods) {
            int modifiers= each.getMethod().getModifiers();
            if (Modifier.isStatic(modifiers) && Modifier.isPublic(modifiers))
                return each;
        }

        throw new Exception("No public static parameters method on class "
                + testClass.getName());
    }
}
