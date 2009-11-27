/**
 * 
 */
package ch.ethz.intervals;

import java.util.Collections;
import java.util.Iterator;


public class EdgeList {
	
	private static final int BITS = 0;	
	private static final int NEXT = 1;	
	private static final int OFFSET = 2;	
	private static final int CHUNK_SIZE = 3;	
	private static final int ARRAY_SIZE = CHUNK_SIZE + OFFSET;
	
	private static int bits(Object[] list) {
		return ((Integer)list[BITS]).intValue();
	}

	/** Returns the number of elements in the list chunk {@code list}.
	 *  Does not count elements in successor chunks.  Used when saving
	 *  the value of an edge list. */
	public static int chunkLen(Object[] list) {
		if(list == null)
			return 0;

		for(int l = CHUNK_SIZE; l > 1; l--)
			if(list[l - 1 + OFFSET] != null)
				return l;
		
		return 1;
	}
	
	public static Object[] add(Object[] list, PointImpl toPoint, boolean deterministic) {
		int bit = (deterministic ? 1 : 0);
				
		if(list != null) {
			for(int i = 1; i < CHUNK_SIZE; i++) {
				if(list[i+OFFSET] == null) {
					list[BITS] = Integer.valueOf(bits(list) | (bit << i));
					list[i+OFFSET] = toPoint;
					return list;
				}
			}
		}
		
		Object[] newList = new Object[ARRAY_SIZE];
		newList[BITS] = Integer.valueOf(bit);
		newList[NEXT] = list;
		newList[OFFSET] = toPoint;
		return newList;
	}
	
	public static Iterable<PointImpl> edges(
			final Object[] list0,
			final boolean onlyDeterministic)
	{
		return edges(list0, CHUNK_SIZE, onlyDeterministic);
	}
	
	public static Iterable<PointImpl> edges(
			final Object[] list0,
			final int maxIdx0,
			final boolean onlyDeterministic) 
	{
		if(list0 == null)
			return Collections.emptyList();
		
		class EdgeListIterator implements Iterator<PointImpl> {
			Object[] list;
			int bits;
			int idx, maxIdx;
			
			public EdgeListIterator(Object[] list, int maxIdx) {
				loadList(list, maxIdx);
				hasNext();
			}
			
			private boolean loadList(Object[] list, int maxIdx) {
				if(list != null) {
					this.list = list;
					this.bits = bits(list);
					this.idx = 0;
					this.maxIdx = maxIdx;
					return true;
				} else {
					this.list = null;
					return false;
				}
			}

			private boolean deterministic() {
				int mask = (1 << idx);
				return (bits & mask) != 0;
			}
			
			private PointImpl get() {
				return (PointImpl) list[idx + OFFSET];
			}			
			
			private boolean advance() {
				idx += 1;
				if(idx == maxIdx || get() == null) {
					Object[] nextList = (Object[])list[NEXT];
					if(!loadList(nextList, CHUNK_SIZE))
						return false;				
				} 
				return true;					
			}
			
			@Override
			public boolean hasNext() {
				if(list == null)
					return false;
				
				if(onlyDeterministic) {
					while(!deterministic())
						if(!advance())
							return false;
				}
				
				return get() != null;
			}
			
			@Override
			public PointImpl next() {
				PointImpl g = get();
				advance();
				return g; 
			}
			
			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}			
		}
		
		return new Iterable<PointImpl>() {
			@Override
			public Iterator<PointImpl> iterator() {
				return new EdgeListIterator(list0, maxIdx0);
			}
		};
	}
		
}