import checkers.nullness.quals.*;
// @skip-test
public class FlowConstructor {

    String a;
    String b;

    public FlowConstructor(int p) {
        a = "m";
        b = "n";
        nonRawMethod();
    }

    public FlowConstructor(double p) {
        a = "m";
        nonRawMethod();  // error
    }

    void nonRawMethod() {
        a.toString();
        b.toString();
    }
}
