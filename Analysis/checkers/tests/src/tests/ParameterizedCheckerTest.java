package tests;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CheckerParameterized.class)
public abstract class ParameterizedCheckerTest extends CheckerTest {
    private final File testFile;

    public ParameterizedCheckerTest(File testFile,
            String checkerName, String checkerDir, String... checkerOptions) {
        super(checkerName, checkerDir, checkerOptions);
        this.testFile = testFile;
    }

    @Test public void run()     {
        test(testFile);
    }

    protected static Collection<Object[]> testFiles(String folder) {
        File dir = new File("tests" + File.separator + folder);
        List<File> javaFiles = TestUtilities.deeplyEnclosedJavaTestFiles(dir);
        Collection<Object[]> arguments = new ArrayList<Object[]>(javaFiles.size());

        for (File javaFile : javaFiles) {
            arguments.add(new Object[] { javaFile });
        }
        return arguments;
    }
}
