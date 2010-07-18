package test.check;

import ch.ethz.intervals.*;
import ch.ethz.intervals.guard.*;

import harmonic.lang.StaticCheck;

class ScopedLock 
implements StaticGuard 
{
    
    public final RoLock lock;
    public final RoInterval scope;

    public ScopedLock(
        RoLock lock,
        RoInterval scope
    ) {
        this.lock = lock;
        this.scope = scope;
    }
    
    @StaticCheck
    public static ScopedLock create(RoLock lock, RoInterval scope) {
        return new ScopedLock(lock, scope);
    }
    
    @Override
    public RuntimeException checkWritable(RoPoint mr, RoInterval inter) {
        if(inter.isSubintervalOfOrEqualTo(scope))
            return lock.checkWritable(mr, inter);
            
        return new RuntimeException("Not a subinterval of " + scope);
    }
    
    @Override
    public RuntimeException checkReadable(RoPoint mr, RoInterval inter) {
        if(scope.getEnd().hbeq(mr))
            return null;
            
        return checkWritable(mr, inter);
    }
    
    @Override
    public RuntimeException ensuresFinal(RoPoint mr, RoInterval inter) {
        if(scope.getEnd().hbeq(mr))
            return null;
            
        return new RuntimeException("Does not happen after " + scope);
    }

    @Override
    public RuntimeException checkLockable(RoInterval inter, RoLock lock) {
        return scope.checkLockable(inter, lock);
    }
    
}