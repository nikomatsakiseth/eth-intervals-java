package ch.ethz.intervals;

/**
 * Dynamic guards monitor field accesses dynamically to guarantee 
 * that no race conditions occur.  They require that all writes
 * to data sharing the same dynamic guard <em>happen before</em>
 * all other writes as well as any reads.  
 * 
 * <p>Whenever there are multiple reads which are not ordered by
 * <i>happens before</i> relations, the next write must occur
 * after the mutual bound of all reads, as defined by
 * {@link Point#mutualBound(Point)}.
 */
public class DynamicGuard 
extends Lock
implements Guard {
	
	private enum StateKind {
		/** Both wr, rd are {@code null} */
		INITIAL,
		
		/** wr is bound of locker, rd is {@code null} */
		LOCK_OWNED,
		
		/** wr is bound of writer, rd is {@code null} */
		WR_OWNED,
		
		/** wr is bound of previous writer, rd is bound of singular reader */
		RD_OWNED, 
		
		/** wr is bound of previous writer, rd is mutual bound of <b>all readers</b> */
		RD_SHARED 
	}
	
	private enum AccessKind {
		LOCK(StateKind.LOCK_OWNED), WR(StateKind.WR_OWNED), RD(StateKind.RD_OWNED);
		
		final StateKind ownerStateKind;

		private AccessKind(StateKind ownerStateKind) {
			this.ownerStateKind = ownerStateKind;
		}		
	}
	
	private static class State {
		StateKind kind;
		Point bound, prevWr;
		State next;
		
		public State(StateKind kind, Point bound, Point prevWr, State next) {
			this.kind = kind;
			this.bound = bound;
			this.prevWr = prevWr;
			this.next = next;
		}
		
		private boolean isRepeat(Point end) {
			return end.line == bound.line && end.isBoundedByOrEqualTo(bound);
		}

	}
	
	private static final State initialState = new State(StateKind.INITIAL, null, null, null);
	
	private State state = initialState;
	
	/** Tries to push a new ownership state of the correct kind for the given
	 *  access.  This is only possible if {@code inter} is bounded by
	 *  the current owner.  Returns false otherwise. */
	private boolean tryPushState(AccessKind accKind, Point mr, Point end) {
		if(end.isBoundedBy(state.bound)) {
			state = new State(accKind.ownerStateKind, end, null, state);
			return true;
		}
		return false;		
	}
	
	/** Returns true if the current state is guaranteed to have finished
	 *  because {@code inter} is active. */
	private boolean canPopState(Point mr) {
		return (state.bound.hbeq(mr));
	}

	/** Pops off the top-most state and retries the access. */
	private boolean popState(AccessKind accKind, Point mr, Point end) {
		state = state.next;
		switch(accKind) {
			case LOCK:
				return isLockableBy(mr, end);
			case WR:
				return isWritableBy(mr, end);
			case RD:
				return isReadableBy(mr, end);
			default:
				throw new RuntimeException("Invalid pushOrPopState: "+accKind); 
		}
	}
	
	/** Pops off the top-most state if it is guaranteed to have finished,
	 *  otherwise just fails.  Not called tryPopState() because a false return
	 *  does not necessarily indicate that it failed to pop the state! */
	private boolean popStateElseFail(AccessKind accKind, Point mr, Point end) {
		if(canPopState(mr))
			return popState(accKind, mr, end);
		return false;
	}
	
	/** Tries to push or pop the state, failing if neither is possible.  
	 *  Note that a false return does not indicate that it failed to push/pop. */
	private boolean pushElsePopStateElseFail(AccessKind accKind, Point mr, Point end) {
		return tryPushState(accKind, mr, end) || popStateElseFail(accKind, mr, end);
	}
	
	/** False if {@code inter} races with the previous write */
	private boolean canGeneralizeReadState(Point mr) {
		return state.prevWr == null || state.prevWr.hbeq(mr);
	}
	
	/** Removes one or more {@link StateKind#RD_OWNED} states from the top of the stack
	 *  and converts to {@link StateKind#RD_SHARED}. */
	private void generalizeReadOwnedState(Point end) {
		assert state.kind == StateKind.RD_OWNED && !end.isBoundedBy(state.bound);
		
		Point prevWr = state.prevWr;
		Point rdBound;
				
		State topMost = state;
		do {		
			rdBound = state.bound;
			topMost = topMost.next;					
		} while(topMost.kind == StateKind.RD_OWNED && !end.isBoundedBy(state.bound));
		
		rdBound = rdBound.mutualBound(end);
		
		state = new State(StateKind.RD_SHARED, rdBound, prevWr, topMost);
	}
		
	/** Generalizes the bound of the {@link StateKind#RD_SHARED} state on top of
	 *  the stack. */
	private void generalizeReadSharedState(Point end) {
		assert state.kind == StateKind.RD_SHARED;
		
		state.bound = state.bound.mutualBound(end);
	}	

	/** True if LOCKing this object is permitted by {@code inter}.
	 *  Invoked only after {@code inter} has successfully acquired the lock.
	 *  Note that just because {@code inter} acquired the lock does not
	 *  mean it was safe to do so: there may be other intervals out there
	 *  doing unlocked accesses and racing with {@code inter}. */
	synchronized boolean isLockableBy(Point mr, Point end) {
		switch(state.kind) {
		case INITIAL:
			state = new State(StateKind.LOCK_OWNED, end, null, state);
			return true;
			
		case LOCK_OWNED:
			// Note: if this is not a recursive lock, then it
			// cannot have happened unless the previous owner
			// had finished with its lock.  This is true even
			// if there is no HB relationship between {@code inter}
			// and the previous owner.
			
			if(tryPushState(AccessKind.LOCK, mr, end))
				return true;
			return popState(AccessKind.LOCK, mr, end);
			
		case WR_OWNED:
			return popStateElseFail(AccessKind.LOCK, mr, end);
			
		case RD_OWNED:
			return popStateElseFail(AccessKind.LOCK, mr, end);
		
		case RD_SHARED:
			return popStateElseFail(AccessKind.LOCK, mr, end);
			
		default:
			throw new RuntimeException("Unhandled state kind: " + state.kind);
		}		
	}

	/**
	 * Returns true if the current interval is permitted to write to
	 * fields guarded by {@code this}.  If this call returns true
	 * once during a given interval, it will always return true.
	 */
	@Override
	public boolean isWritable() {		
		Current current = Current.get();
		
		// Root interval:
		//     For now play it safe.  Later think about this.
		if(current.inter == null)
			return false;
		
		return isWritableBy(current.mr, current.inter.end);
	}
	
	synchronized private boolean isWritableBy(Point mr, Point end) {
		switch(state.kind) {
		case INITIAL:
			state = new State(StateKind.WR_OWNED, end, null, state);
			return true;
			
		case LOCK_OWNED:
			if(state.isRepeat(end))
				return true;
			return pushElsePopStateElseFail(AccessKind.WR, mr, end);
			
		case WR_OWNED:
			if(state.isRepeat(end))
				return true;
			return pushElsePopStateElseFail(AccessKind.WR, mr, end);
			
		case RD_OWNED:
			if(state.isRepeat(end)) {
				state.kind = StateKind.WR_OWNED;
				state.prevWr = null; // no longer relevant
				return true;
			}
			return pushElsePopStateElseFail(AccessKind.WR, mr, end);
		
		case RD_SHARED:
			if(canPopState(mr))
				return popState(AccessKind.WR, mr, end);
			return false;
			
		default:
			throw new RuntimeException("Unhandled state kind: " + state.kind);
		}		
	}
	
	@Override
	public boolean isReadable() {
		Current current = Current.get();		
		
		// Root interval:
		//     For now play it safe.  Later think.
		if(current.inter == null)
			return false;
		
		return isReadableBy(current.mr, current.inter.end);
	}
	
	synchronized boolean isReadableBy(Point mr, Point end) {
		switch(state.kind) {
		case INITIAL: 
			state = new State(StateKind.RD_OWNED, end, null, state);
			return true;
			
		case LOCK_OWNED:
			if(state.isRepeat(end))
				return true;
			
			return pushElsePopStateElseFail(AccessKind.RD, mr, end);			
		
		case WR_OWNED: 
			if(state.isRepeat(end))
				return true;
			
			return pushElsePopStateElseFail(AccessKind.RD, mr, end);
			
		case RD_OWNED:
			if(state.isRepeat(end))
				return true;
			if(tryPushState(AccessKind.RD, mr, end))
				return true;
			if(canPopState(mr))
				return popState(AccessKind.RD, mr, end);
			if(canGeneralizeReadState(mr)) {
				generalizeReadOwnedState(end);
				return true;
			}
			return false;
			
		case RD_SHARED:
			if(canPopState(mr))
				return popState(AccessKind.RD, mr, end);
			if(canGeneralizeReadState(mr)) {
				generalizeReadSharedState(end);
				return true;
			}
			return false;
			
		default:
			throw new RuntimeException("Unhandled state kind: " + state.kind);
		}
	}

}
