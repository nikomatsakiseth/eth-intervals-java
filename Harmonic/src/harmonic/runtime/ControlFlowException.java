package harmonic.runtime;

/** Base class for all Control Flow exceptions. */
public abstract class ControlFlowException extends RuntimeException {
    public ControlFlowException fillInStackTrace() {
        return this;
    }
}