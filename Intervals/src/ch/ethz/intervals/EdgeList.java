/**
 * 
 */
package ch.ethz.intervals;

import java.util.Collections;
import java.util.Iterator;


public class EdgeList {
	
	static private final int ALL_POINTS_MASK = 7; 
	static private final int CHUNK_LEN = 3; 
	
	private int bits;
	private PointImpl toPoint0;
	private PointImpl toPoint1;
	private PointImpl toPoint2;
	private EdgeList next;
	
	private EdgeList(PointImpl pnt0, int bits, EdgeList next) {
		this.bits = bits;
		this.toPoint0 = pnt0;
		this.next = next;
	}
	
	private boolean add(PointImpl toPoint, int bit) {
		if(toPoint0 == null) {
			toPoint0 = toPoint;
			bits |= (bit);
			return true;
		} else if(toPoint1 == null) {
			toPoint1 = toPoint;
			bits |= (bit << 1);
			return true;
		} else if(toPoint2 == null) {
			toPoint2 = toPoint;
			bits |= (bit << 2);
			return true;
		} else {
			return false;
		}
	}
	
	public static int chunkMask(EdgeList list) {
		int mask = 0;
		if(list != null) {
			if(list.toPoint0 != null) mask |= 1;
			if(list.toPoint1 != null) mask |= 2;
			if(list.toPoint2 != null) mask |= 4;
		}
		return mask;
	}
	
	public static EdgeList add(EdgeList list, PointImpl toPoint, boolean deterministic) {
		int bit = (deterministic ? 1 : 0);
		if(list != null && list.add(toPoint, bit))
			return list;
		return new EdgeList(toPoint, bit, list);		
	}
	
	public static Iterable<PointImpl> edges(
			final EdgeList list,
			final boolean onlyDeterministic)
	{
		return edges(list, ALL_POINTS_MASK, onlyDeterministic);
	}

	public static void remove(EdgeList list, PointImpl toImpl) {
		for(; list != null; list = list.next) {
			if(list.toPoint0 == toImpl) {
				list.toPoint0 = null;
				list.bits &= ~1;
				return;
			} else if(list.toPoint1 == toImpl) {
				list.toPoint1 = null;
				list.bits &= ~2;
				return;
			} else if(list.toPoint2 == toImpl) {
				list.toPoint2 = null;
				list.bits &= ~4;
				return;
			}
		}
	}

	public static void setDeterministic(EdgeList list, PointImpl toImpl) {
		for(; list != null; list = list.next) {
			if(list.toPoint0 == toImpl) {
				list.bits |= 1;
				return;
			} else if(list.toPoint1 == toImpl) {
				list.bits |= 2;
				return;
			} else if(list.toPoint2 == toImpl) {
				list.bits |= 4;
				return;
			}
		}
	}
	
	public static Iterable<PointImpl> edges(
			final EdgeList list0,
			final int mask0,
			final boolean onlyDeterministic) 
	{
		if(list0 == null)
			return Collections.emptyList();
		
		class EdgeListIterator implements Iterator<PointImpl> {
			EdgeList list;
			int idx,  idxMask;
			
			public EdgeListIterator(EdgeList list, int idxMask) {
				loadList(list, idxMask);
			}
			
			private boolean deterministic() {
				int mask = (1 << idx);
				return (list.bits & mask) != 0;
			}
			
			private PointImpl get() {
				if(idx == 0 && (idxMask & 1) != 0) return list.toPoint0;
				if(idx == 1 && (idxMask & 2) != 0) return list.toPoint1;
				if(idx == 2 && (idxMask & 4) != 0) return list.toPoint2;
				return null;
			}			
			
			private boolean loadList(EdgeList list, int idxMask) {
				if(list != null) {
					this.list = list;
					this.idx = CHUNK_LEN;
					this.idxMask = idxMask;
					return advance();
				} else {
					this.list = null;
					return false;
				}
			}

			/** Moves along the list until it finds a suitable, non-null entry. */
			private boolean advance() {
				do {
					if(--idx == -1)
						return loadList(list.next, ALL_POINTS_MASK);
				} while(get() == null || (onlyDeterministic && !deterministic()));
				return true;					
			}
			
			@Override
			public boolean hasNext() {
				return list != null;
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
				return new EdgeListIterator(list0, mask0);
			}
		};
	}

}