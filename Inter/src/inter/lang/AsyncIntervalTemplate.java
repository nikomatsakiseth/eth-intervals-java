package inter.lang;

interface AsyncIntervalTemplate<R, A> extends IntervalTemplate<R, A> {

    @Requires(@Requirement("method subOf Parent"))
    public R value(A argument);
    
}