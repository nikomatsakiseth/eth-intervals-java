package harmonic.runtime;

/** The exception thrown when a block in 
  * harmonic code executes a return statement.
  * Caught by the method containing the block. */
public class Return extends ControlFlowException {
    // Note: this field name is hard-coded
    // into ByteCode.scala, so don't change it.
    public final Object value;
    
    public Return(Object value) {
        this.value = value;
    }
}