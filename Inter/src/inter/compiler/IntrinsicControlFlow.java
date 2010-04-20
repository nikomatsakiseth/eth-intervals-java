package inter.compiler;

import inter.lang.IntervalTemplate;

class IntrinsicControlFlow {
    
    public static Void if_(
        Boolean value,
        IntervalTemplate<Void, Void> ifTmpl
    ) {
        if(value) 
            ifTmpl.value(null);
        return null;
    }
    
    public static Void ifNull(
        Object value,
        IntervalTemplate<Void,Void> ifTmpl
    ) {
        if(value != null) {
            ifTmpl.value(null);
        } 
        return null;
    }
    
    public static <R> R ifElse(
        Boolean value,
        IntervalTemplate<R,Void> ifTmpl, 
        IntervalTemplate<R,Void> elseTmpl
    ) {
        if(value) {
            return ifTmpl.value(null);
        } else {
            return elseTmpl.value(null);
        }
    }

    public static <R> R ifNullElse(
        Object value,
        IntervalTemplate<R,Void> ifTmpl,
        IntervalTemplate<R,Void> elseTmpl
    ) {
        if(value != null) {
            return ifTmpl.value(null);
        } else {
            return elseTmpl.value(null);
        }
    }

    public static <Void, A> Void forEach(
        Iterable<A> iterable,
        IntervalTemplate<Void,A> eachTmpl
    ) {
        for(A item : iterable)
            eachTmpl.value(item);
        return null;
    }
    
}
