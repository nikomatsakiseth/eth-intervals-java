package inter.compiler;

interface IntervalTemplate<R, A> {
    
    @Requires(@Requirement("method inlineSubOf Parent"))
    R value(A argument);
    
}