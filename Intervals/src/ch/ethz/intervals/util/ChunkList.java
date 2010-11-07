package ch.ethz.intervals.util;

/**
 * A utility class for efficiently representing lists of items along with
 * a few associated flag bits.  The lists resemble functional linked lists
 * in many ways, but they are not immutable.  They consist of a linked list of
 * chunks, each chunk stores a small, fixed number of elements.  This drastically
 * improves memory consumption for longer lists, while still being very concise for
 * short lists.  The empty list is represented simply by a {@code null} pointer.
 * Use the static method {@link ChunkList#add(ChunkList, Object, int)}
 * to prepend objects to the list (appending is not supported).  Iteration is performed
 * by instantiating an anonymous subtype of 
 * {@link ChunkList.Iterator} or {@link ChunkList.InterruptibleIterator}.
 */
public class ChunkList<T> implements Cloneable {
	
	// Flags for edges:
	public static final int NORMAL = 0;               /** A user-created, confirmed edge. */   
	static public final int SPECULATIVE = 1;          /** Not yet confirmed. */
	static public final int TEST_EDGE = 2;            /** Edges used in the tests to force ordering which DO NOT COUNT FOR HB. */
	
	// Flags for intervals
	public static final int NO_FLAGS = 0;
	
	static public final int CNT_FLAGS = 10;            			/** Number of flag bits we can support per entry. */
	static public final int ALL_FLAGS = (1 << CNT_FLAGS) - 1; 	/** Union of all flags. */

	static public final boolean speculative(int flags) {
		return (flags & SPECULATIVE) != 0;
	}
	
	static public final boolean tests(int flags) {
		return (flags & TEST_EDGE) != 0;
	}
	
	static private final int SHIFT0 = 0;
	static private final int SHIFT1 = CNT_FLAGS; 
	static private final int SHIFT2 = CNT_FLAGS*2;
	
	private int flags;
	private T mem0;
	private T mem1;
	private T mem2;
	private ChunkList<T> next;
	
	private ChunkList(T pnt0, int flags, ChunkList<T> next) {
		this.flags = flags;
		this.mem0 = pnt0;
		this.next = next;
	}
	
	private boolean add(T toPoint, int toFlags) {
		if(mem0 == null) {
			mem0 = toPoint;
			flags |= toFlags << SHIFT0;
			return true;
		} else if(mem1 == null) {
			mem1 = toPoint;
			flags |= toFlags << SHIFT1;
			return true;
		} else if(mem2 == null) {
			mem2 = toPoint;
			flags |= toFlags << SHIFT2;
			return true;
		} else {
			return false;
		}
	}
	
	public static <T> ChunkList<T> empty() {
		return new ChunkList<T>(null, 0, null);
	}
			
	public static <T> ChunkList<T> add(ChunkList<T> list, T toPoint, int toFlags) {
		assert (toFlags & ALL_FLAGS) == toFlags; // No extra bits.
		if(list != null && list.add(toPoint, toFlags))
			return list;
		return new ChunkList<T>(toPoint, toFlags, list);		
	}
	
	public static <T> void remove(ChunkList<T> list, T toImpl) {
		for(; list != null; list = list.next) {
			if(list.mem0 == toImpl) {
				list.mem0 = null;
				list.flags &= ~(ALL_FLAGS << SHIFT0);
				return;
			} else if(list.mem1 == toImpl) {
				list.mem1 = null;
				list.flags &= ~(ALL_FLAGS << SHIFT1);
				return;
			} else if(list.mem2 == toImpl) {
				list.mem2 = null;
				list.flags &= ~(ALL_FLAGS << SHIFT2);
				return;
			}
		}
	}
	
	/** Finds a speculative edge to toImpl and clears the speculative bit,
	 *  also adding the flags in {@code addFlags}. */
	public static <T> void removeSpeculativeFlagAndAdd(
			ChunkList<T> list, 
			T toImpl, 
			int addFlags) 
	{
		assert (addFlags & ~ALL_FLAGS) == 0;
		
		for(; list != null; list = list.next) {
			int flags = list.flags;
			if(list.mem0 == toImpl && speculative(flags >> SHIFT0)) {
				flags &= ~(ChunkList.SPECULATIVE << SHIFT0);
				flags |= (addFlags << SHIFT0);
				list.flags = flags;
				return;
			} else if(list.mem1 == toImpl && speculative(flags >> SHIFT1)) {
				flags &= ~(ChunkList.SPECULATIVE << SHIFT1);
				flags |= (addFlags << SHIFT1);
				list.flags = flags;
				return;
			} else if(list.mem2 == toImpl && speculative(flags >> SHIFT2)) {
				flags &= ~(ChunkList.SPECULATIVE << SHIFT2);
				flags |= (addFlags << SHIFT2);
				list.flags = flags;
				return;
			}
		}
		assert false : "No speculative edge found!";
	}
	
	/**
	 * Used to iterate through a {@link ChunkList}.  Use like so:
	 * <pre>
	 *   ChunkList<T> theList;
	 *   new ChunkList.Iterator<T>(theList) {
	 *     public boolean forEach(T item, int flags) {
	 *       if(should break out of the loop)
	 *         return true;
	 *       else if(should continue to the next item)
	 *         return false; 
	 *     }
	 *   }
	 * </pre>
	 */
	public static abstract class InterruptibleIterator<T> {
		
		public InterruptibleIterator(ChunkList<T> list) {
			while(list != null) {
				int flags = list.flags;
				
				if(list.mem0 != null && forEach(list.mem0, flags >> SHIFT0))
					break;
				
				if(list.mem1 != null && forEach(list.mem1, flags >> SHIFT1))
					break;
				
				if(list.mem2 != null && forEach(list.mem2, flags >> SHIFT2))
					break;				
				
				list = list.next;
			}
		}

		/**
		 * Invoked for each point with the corresponding flags.
		 * Note that the flags int may include bits outside of
		 * {@link ChunkList#ALL_FLAGS} so be sure to mask if needed.
		 * @return true if we should break out of the loop
		 */
		public abstract boolean forEach(T toPoint, int flags);
		
	}

	/**
	 * An iterator that does not support breaking out of the loop.  
	 * Use like so:
	 * <pre>
	 *   ChunkList<T> theList;
	 *   new ChunkList.Iterator<T>(theList) {
	 *     public void doForEach(T item, int flags) { ... }
	 *   }
	 * </pre>
	 */
	public static abstract class Iterator<T> extends InterruptibleIterator<T> {
		
		public Iterator(ChunkList<T> list) {
			super(list);
		}

		/** Like {@link #forEach(Object, int)} but with
		 *  no possibility to break out of the loop. */
		public abstract void doForEach(T toPoint, int flags);

		@Override
		public final boolean forEach(T toPoint, int flags) {
			doForEach(toPoint, flags);
			return false;
		}
	}

}