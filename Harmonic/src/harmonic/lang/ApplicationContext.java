package harmonic.lang;

import java.io.PrintStream;
import java.io.InputStream;
import ch.ethz.intervals.Interval;

/** The context provided to an Application instance. */
public interface ApplicationContext {

    /** Stream for stdin */
    InputStream getIn();
    
    /** Stream for stdout */
    PrintStream getOut();
    
    /** Stream for stderr */
    PrintStream getErr();
    
    /** Arguments from the command line */
    String[] getArgs();
    
    /** Root interval for this application */
    Interval getRoot();
    
}