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
		INITIAL,		
		LOCK_OWNED,		
		WR_OWNED,		
		RD_OWNED, 		
		RD_SHARED,		
		EMBEDDED
	}
	
	private enum AccessKind {
		LOCK(StateKind.LOCK_OWNED), WR(StateKind.WR_OWNED), RD(StateKind.RD_OWNED), EMBED(StateKind.EMBEDDED);
		
		final StateKind ownerStateKind;

		private AccessKind(StateKind ownerStateKind) {
			this.ownerStateKind = ownerStateKind;
		}		
	}
	
	private static class State {
		
		/** Characterizes the access that brought us into this state */
		StateKind kind;
		
		/** End of the interval which performed the access, or mutual
		 *  bound of the interval(s) in state {@link StateKind#RD_SHARED}.
		 *  {@code null} for the state {@link StateKind#INITIAL} */
		Point bound;
		
		/** Interior point (hb bound) where the last write occurred. */
		Point prevWr;
		
		/** If {@link StateKind#EMBEDDED}, the guard of the data structure
		 *  in which we are embedded.  Else {@code null}. */
		Guard embeddedIn;
		
		/** Previous state 
		 *  (Invariant: bound {@link Point#isBoundedByOrEqualTo(Point)} next.bound) */
		State next;
		
		public State(StateKind kind, Point bound, Point prevWr, State next) {
			this(kind, bound, prevWr, null, next);
		}
		
		public State(StateKind kind, Point bound, State next) {
			this(kind, bound, null, null, next);
		}
		
		public State(Guard embeddedIn, State next) {
			this(StateKind.EMBEDDED, null, null, embeddedIn, next);
		}
		
		public State(StateKind kind, Point bound, Point prevWr, Guard embeddedIn, State next) {
			this.kind = kind;
			this.bound = bound;
			this.prevWr = prevWr;
			this.embeddedIn = embeddedIn;
			this.next = next;
		}
		
		private boolean isMutating() {
			switch(kind) {
			case INITIAL: 
			case RD_OWNED:
				return prevWr != null;
				
			case WR_OWNED:
			case LOCK_OWNED:
				return true;
				
			case RD_SHARED:
			case EMBEDDED:
				return false;
				
			default:
				assert false : "Unhandle state kind: " + kind;
				return false;
			}
		}
		
		final boolean isRepeatFromSameLine(Point end) {
			// Strictly speaking, the correct code here would be end == bound,
			// which essentially means the interval is precisely the same.  However,
			// we accept blocking subintervals as well because it permits us
			// to push fewer states onto the stack.  I believe the resulting
			// machines are equivalent in terms of what they accept.
			return end.line == bound.line && end.isBoundedByOrEqualTo(bound);
		}

		final boolean isSameInterval(Point end) {
			return end == bound;
		}

	}
	
	private State state = new State(StateKind.INITIAL, null, null, null);
	
	private boolean canPushState(Point end) {
		return end.isBoundedBy(state.bound);
	}
	
	/** Tries to push a new ownership state of the correct kind for the given
	 *  access.  This is only possible if {@code inter} is bounded by
	 *  the current owner.  Returns false otherwise. */
	private boolean tryPushState(Point mr, Point end, AccessKind accKind, Point bound, Guard embeddedIn) {
		if(canPushState(end)) {
			// Subtle: prevWr is always null here.  Why?  Because even if the parent 
			// was writing, they are suspended now, and we are their child, so their
			// latest write must HB our start (and the start of any reads/writes which
			// this state will authorize).
			state = new State(accKind.ownerStateKind, bound, null, embeddedIn, state);
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
	private boolean popStateAndRetry(AccessKind accKind, Point mr, Point end, Guard embedIn) {		
		prepareToPopStack(state);		
		state = state.next;
		switch(accKind) {
			case LOCK:
				return isLockableBy(mr, end);
			case WR:
				return isWritableBy(mr, end);
			case RD:
				return isReadableBy(mr, end);
			case EMBED:
				return embed(mr, end, embedIn);
			default:
				throw new RuntimeException("Invalid pushOrPopState: "+accKind); 
		}
	}

	/** Pops off the top-most state if it is guaranteed to have finished,
	 *  otherwise just fails.  Not called tryPopState() because a false return
	 *  does not necessarily indicate that it failed to pop the state! */
	private boolean popStateAndRetryElseFail(AccessKind accKind, Point mr, Point end, Guard embedIn) {
		if(canPopState(mr))
			return popStateAndRetry(accKind, mr, end, embedIn);
		return false;
	}
	
	/** Tries to push or pop the state, failing if neither is possible.  
	 *  Note that a false return does not indicate that it failed to push/pop. */
	private boolean pushElsePopStateAndRetryElseFail(Point mr, Point end, AccessKind accKind, Point bound, Guard embedIn) {
		return tryPushState(mr, end, accKind, bound, embedIn) || popStateAndRetryElseFail(accKind, mr, end, embedIn);
	}
	
	/** False if {@code inter} races with the previous write */
	private boolean canGeneralizeReadState(Point mr) {
		return state.prevWr == null || state.prevWr.hbeq(mr);
	}
	
	/** Removes one or more {@link StateKind#RD_OWNED} states from the top of the stack
	 *  and converts to {@link StateKind#RD_SHARED}. */
	private boolean tryGeneralizeReadOwnedState(Point mr, Point end) {
		assert state.kind == StateKind.RD_OWNED && !end.isBoundedBy(state.bound);
		
		// Walk up the stack to identify those RD_OWNED states 
		// which will be removed.  We want to remove any state
		// that does not bound 'end'.  Along the way, we update
		// prevWr pointers.  Now, normally it's a no-no to update
		// states before an access is found safe, but in this case
		// the updates are not due to the current read access but 
		// rather prior write accesses which we DID find safe.
		// In the worst case, we won't change the stack, and we will
		// have propagated 'prevWr' a little early.
		
		Point rdBound;
				
		State topMost = state;
		do {
			prepareToPopStack(topMost);
			rdBound = state.bound;
			topMost = topMost.next;					
		} while(topMost.kind == StateKind.RD_OWNED && !end.isBoundedBy(state.bound));
			
		if(topMost.prevWr != null)
			if(!topMost.prevWr.hbeq(mr))
				return false;
		
		rdBound = rdBound.mutualBound(end);
		
		state = new State(StateKind.RD_SHARED, rdBound, topMost.prevWr, topMost);
		return true;
	}
		
	/** Invoked by {@link #popStateAndRetry(AccessKind, Point, Point, Guard)} and
	 *  {@link #tryGeneralizeReadOwnedState(Point, Point)} before they pop a state
	 *  (or contemplate popping a state) from the stack.  {@code top} is the state
	 *  to be popped. */
	private void prepareToPopStack(State top) {
		// Update the prevWr field of the parent state.
		if(top.isMutating())
			top.next.prevWr = top.bound;
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
			
			if(tryPushState(mr, end, AccessKind.LOCK, end, null))
				return true;
			return popStateAndRetry(AccessKind.LOCK, mr, end, null);
			
		case WR_OWNED:
			return popStateAndRetryElseFail(AccessKind.LOCK, mr, end, null);
			
		case RD_OWNED:
			return popStateAndRetryElseFail(AccessKind.LOCK, mr, end, null);
		
		case RD_SHARED:
			return popStateAndRetryElseFail(AccessKind.LOCK, mr, end, null);
			
		case EMBEDDED:
			return false;
			
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
		case WR_OWNED:
			if(state.isRepeatFromSameLine(end))
				return true;
			return pushElsePopStateAndRetryElseFail(mr, end, AccessKind.WR, end, null);
			
		case RD_OWNED:
			if(state.isSameInterval(end)) { // XXX not really sound I think unless end == state.bound
				state.kind = StateKind.WR_OWNED;
				state.prevWr = null; // no longer relevant
				return true;
			}
			return pushElsePopStateAndRetryElseFail(mr, end, AccessKind.WR, end, null);
		
		case RD_SHARED:
			if(canPopState(mr))
				return popStateAndRetry(AccessKind.WR, mr, end, null);
			return false;
			
		case EMBEDDED:
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
			if(state.isRepeatFromSameLine(end))
				return true;
			
			return pushElsePopStateAndRetryElseFail(mr, end, AccessKind.RD, end, null);			
		
		case WR_OWNED: 
			if(state.isRepeatFromSameLine(end))
				return true;
			
			return pushElsePopStateAndRetryElseFail(mr, end, AccessKind.RD, end, null);
			
		case RD_OWNED:
			if(state.isRepeatFromSameLine(end))
				return true;
			if(tryPushState(mr, end, AccessKind.RD, end, null))
				return true;
			if(canPopState(mr))
				return popStateAndRetry(AccessKind.RD, mr, end, null);
			return tryGeneralizeReadOwnedState(mr, end);
			
		case RD_SHARED:
			if(canPopState(mr))
				return popStateAndRetry(AccessKind.RD, mr, end, null);
			if(state.prevWr == null || state.prevWr.hbeq(mr)) {
				state.bound = state.bound.mutualBound(end);
				return true;
			}
			return false;
			
		case EMBEDDED:
			return false;
			
		default:
			throw new RuntimeException("Unhandled state kind: " + state.kind);
		}
	}
	
	synchronized boolean embed(Point mr, Point end, Guard embedIn) {
		if(!embedIn.isWritable())
			return false;
		
		switch(state.kind) {
		case LOCK_OWNED:
		case WR_OWNED:
		case RD_OWNED:			
		case RD_SHARED:
			return pushElsePopStateAndRetryElseFail(mr, end, AccessKind.EMBED, null, embedIn);
			
		case EMBEDDED:
			return false;
			
		default:
			throw new RuntimeException("Unhandled state kind: " + state.kind);
		}
		
	}
	
	synchronized boolean unembed(Point mr, Point end) {
		switch(state.kind) {
		case EMBEDDED:
			// Unembedding makes the current interval the exclusive owner.
			// It could, if it chose, make writes etc.  Anyone that wants
			// to read it etc must Happen-After the unembedder. 
			state = new State(StateKind.WR_OWNED, end, state);
			return true;
			
		default:
			return false;
		}		
	}

}
