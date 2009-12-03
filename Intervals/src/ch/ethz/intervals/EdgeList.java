/**
 * 
 */
package ch.ethz.intervals;




public class EdgeList implements Cloneable {
	
	// Flags for edges:
	public static final int NORMAL = 0;               /** A user-created, confirmed edge. */   
	static public final int NONDETERMINISTIC = 1;     /** May not exist on every program run. */
	static public final int SPECULATIVE = 2;          /** Not yet confirmed. */
	static public final int WAITING = 4;              /** Waiting for point to occur. */
	static public final int CNT_USER_FLAGS = 3;
	static public final int ALL_USER_FLAGS = (1 << CNT_USER_FLAGS) - 1;

	static private final int EXISTS = 8;              /** Internal flag: Is non-null. */
	static private final int CNT_FLAGS = CNT_USER_FLAGS + 1;
	static private final int ALL_FLAGS = (1 << CNT_FLAGS) - 1;
	
	static public final boolean speculative(int flags) {
		return (flags & SPECULATIVE) != 0;
	}
	
	static public final boolean nondeterministic(int flags) {
		return (flags & NONDETERMINISTIC) != 0;
	}
	
	static public final boolean exists(int flags) {
		return (flags & EXISTS) != 0;
	}
	
	static public final boolean waiting(int flags) {
		return (flags & WAITING) != 0;
	}
	
	static private final int SHIFT0 = 0;
	static private final int SHIFT1 = CNT_FLAGS; 
	static private final int SHIFT2 = CNT_FLAGS*2;
	
	private int flags;
	private PointImpl toPoint0;
	private PointImpl toPoint1;
	private PointImpl toPoint2;
	private EdgeList next;
	
	private EdgeList(PointImpl pnt0, int flags, EdgeList next) {
		assert exists(flags);
		this.flags = flags;
		this.toPoint0 = pnt0;
		this.next = next;
	}
	
	private boolean add(PointImpl toPoint, int toFlags) {
		if(toPoint0 == null) {
			toPoint0 = toPoint;
			flags |= toFlags << SHIFT0;
			return true;
		} else if(toPoint1 == null) {
			toPoint1 = toPoint;
			flags |= toFlags << SHIFT1;
			return true;
		} else if(toPoint2 == null) {
			toPoint2 = toPoint;
			flags |= toFlags << SHIFT2;
			return true;
		} else {
			return false;
		}
	}
	
	public static EdgeList add(EdgeList list, PointImpl toPoint, int toFlags) {
		assert (toFlags & ALL_USER_FLAGS) == toFlags; // No extra bits.
		toFlags |= EXISTS;
		if(list != null && list.add(toPoint, toFlags))
			return list;
		return new EdgeList(toPoint, toFlags, list);		
	}

	public static void remove(EdgeList list, PointImpl toImpl) {
		for(; list != null; list = list.next) {
			if(list.toPoint0 == toImpl) {
				list.toPoint0 = null;
				list.flags &= ~(ALL_FLAGS << SHIFT0);
				return;
			} else if(list.toPoint1 == toImpl) {
				list.toPoint1 = null;
				list.flags &= ~(ALL_FLAGS << SHIFT1);
				return;
			} else if(list.toPoint2 == toImpl) {
				list.toPoint2 = null;
				list.flags &= ~(ALL_FLAGS << SHIFT2);
				return;
			}
		}
	}
	
	/** Finds a speculative edge to toImpl and clears the speculative bit,
	 *  also adding the flags in {@code addFlags}. */
	public static void removeSpeculativeFlagAndAdd(
			EdgeList list, 
			PointImpl toImpl, 
			int addFlags) 
	{
		assert (addFlags & ~ALL_USER_FLAGS) == 0;
		
		for(; list != null; list = list.next) {
			int flags = list.flags;
			if(list.toPoint0 == toImpl && speculative(flags >> SHIFT0)) {
				flags &= ~(EdgeList.SPECULATIVE << SHIFT0);
				flags |= (addFlags << SHIFT0);
				list.flags = flags;
				return;
			} else if(list.toPoint1 == toImpl && speculative(flags >> SHIFT1)) {
				flags &= ~(EdgeList.SPECULATIVE << SHIFT1);
				flags |= (addFlags << SHIFT1);
				list.flags = flags;
				return;
			} else if(list.toPoint2 == toImpl && speculative(flags >> SHIFT2)) {
				flags &= ~(EdgeList.SPECULATIVE << SHIFT2);
				flags |= (addFlags << SHIFT2);
				list.flags = flags;
				return;
			}
		}
		assert false : "No speculative edge found!";
	}
	
	public static abstract class InterruptibleIterator {
		
		public InterruptibleIterator(EdgeList list) {
			while(list != null) {
				int flags = list.flags;
				
				final int flags0 = flags >> SHIFT0;
				if(exists(flags0) && forEach(list.toPoint0, flags0))
					break;
				
				final int flags1 = flags >> SHIFT1;
				if(exists(flags1) && forEach(list.toPoint1, flags1))
					break;
				
				final int flags2 = flags >> SHIFT2;
				if(exists(flags2) && forEach(list.toPoint2, flags2))
					break;				
				
				list = list.next;
			}
		}

		/**
		 * Invoked for each point with the corresponding flags.
		 * Note that the flags int may include bits outside of
		 * {@link EdgeList#ALL_USER_FLAGS} so be sure to mask if needed.
		 * @return true if we should break out of the loop
		 */
		public abstract boolean forEach(PointImpl toPoint, int flags);
		
	}

	public static abstract class Iterator extends InterruptibleIterator {
		
		public Iterator(EdgeList list) {
			super(list);
		}

		/** Like {@link Iterator#forEach(PointImpl, int)} but with
		 *  no possibility to break out of the loop. */
		public abstract void doForEach(PointImpl toPoint, int flags);

		@Override
		public final boolean forEach(PointImpl toPoint, int flags) {
			doForEach(toPoint, flags);
			return false;
		}
	}
		
	
}