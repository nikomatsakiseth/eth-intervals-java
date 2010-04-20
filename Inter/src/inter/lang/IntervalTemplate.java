package inter.lang;

public interface IntervalTemplate<R, A> {
    
    @Requires(@Requirement("method inlineSubOf Parent"))
    R value(A argument);
    
}