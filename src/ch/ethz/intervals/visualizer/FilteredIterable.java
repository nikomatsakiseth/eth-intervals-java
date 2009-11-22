package ch.ethz.intervals.visualizer;

import java.util.Iterator;

public abstract class FilteredIterable<E, F>
implements Iterable<F>
{
	
	private final Iterable<E> iterable;
	
	public FilteredIterable(Iterable<E> list) {
		this.iterable = list;
	}

	class FilteredIterator 
	implements Iterator<F>
	{
		final Iterator<E> iterator;
		boolean loaded;
		F bullet;
		
		public FilteredIterator(Iterator<E> iterator) {
			this.iterator = iterator;
			
			bullet = preload();
			loaded = (bullet != null);
		}

		@Override
		public boolean hasNext() {
			return load();
		}

		@Override
		public F next() {
			load();	
			if(loaded) {
				F result = bullet;
				bullet = null;
				loaded = false;
				return result;				
			}
			throw new RuntimeException("No more items");
		}

		private boolean load() {
			if(loaded) // already loaded
				return true;
			while(iterator.hasNext()) {
				E next = iterator.next();
				if(shouldInclude(next)) {
					bullet = map(next);
					loaded = true;
					return true;
				}
			}
			bullet = null;
			loaded = false;
			return false;
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
		
	}
	
	@Override
	public Iterator<F> iterator() {
		return new FilteredIterator(iterable.iterator());
	}

	protected F preload() {
		// can be overloaded to return a non-null value
		return null;
	}
	abstract protected boolean shouldInclude(E e);
	abstract protected F map(E e);

}
