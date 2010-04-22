package inter.lang;

public interface Block<R, A> {
    
    @Requires(@Requirement("method inlineSubOf Parent"))
    R value(A argument);
    
}