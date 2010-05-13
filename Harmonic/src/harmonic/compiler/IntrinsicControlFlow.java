package harmonic.compiler;

import harmonic.lang.Block;

public class IntrinsicControlFlow {
    
    public static Void if_(
        Boolean value,
        Block<Void, Void> ifTmpl
    ) {
        if(value) 
            ifTmpl.value(null);
        return null;
    }
    
    public static Void ifNull(
        Object value,
        Block<Void,Void> ifTmpl
    ) {
        if(value != null) {
            ifTmpl.value(null);
        } 
        return null;
    }
    
    public static <R> R ifElse(
        Boolean value,
        Block<R,Void> ifTmpl, 
        Block<R,Void> elseTmpl
    ) {
        if(value) {
            return ifTmpl.value(null);
        } else {
            return elseTmpl.value(null);
        }
    }

    public static <R> R ifNullElse(
        Object value,
        Block<R,Void> ifTmpl,
        Block<R,Void> elseTmpl
    ) {
        if(value != null) {
            return ifTmpl.value(null);
        } else {
            return elseTmpl.value(null);
        }
    }

    public static <A> Void forEach(
        Iterable<A> iterable,
        Block<Void,A> eachTmpl
    ) {
        for(A item : iterable)
            eachTmpl.value(item);
        return null;
    }
    
    public static Void while_(
        Block<Boolean, Void> condTmpl,
        Block<Void, Void> bodyTmpl
    ) {
        while(true) {
            if(!condTmpl.value(null))
                return null;
            bodyTmpl.value(null);
        }
    }
    
}
