package harmonic.lang;

public interface AsyncBlock<R, A> extends Block<R, A> {

    @Requires("method subOf Parent")
    public R value(A argument);
    
}