package inter.lang;

public interface AsyncBlock<R, A> extends Block<R, A> {

    @Requires(@Requirement("method subOf Parent"))
    public R value(A argument);
    
}