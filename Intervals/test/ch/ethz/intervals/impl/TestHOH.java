package ch.ethz.intervals.impl;

import static ch.ethz.intervals.Intervals.inline;
import static java.lang.String.format;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.task.AbstractTask;

@DefinesGhost
@interface HOHList {
    public String value() default "HOHList";
}

public class TestHOH {
    
	static class Link 
	extends Object {
		final LockImpl lockImpl;
		int data;
		Link next;
		
		public Link(int data, Link next) {
			this.lockImpl = new LockImpl();
			this.data = data;
			this.next = next;
		}
		
		@Override
		public String toString() {
			return String.format("[%d, %s]", data, next);
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + data;
			result = prime * result + ((next == null) ? 0 : next.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Link other = (Link) obj;
			if (data != other.data)
				return false;
			if (next == null) {
				if (other.next != null)
					return false;
			} else if (!next.equals(other.next))
				return false;
			return true;
		}
	}
	
	interface Transform {
		void transform(Link l);
	}
	
	Interval mapWalk(Interval parent, Link link, Transform transform) {
		Interval inter = parent.newAsyncChild(new MapWalk(link, transform));
		return inter;
	}

	class MapWalk extends AbstractTask {
	    final Link link;
	    final Transform transform;

		public MapWalk(Link link, Transform transform) {
			this.link = link;
			this.transform = transform;
		}
		
		@Override
		public void attachedTo(Interval inter) {
			super.attachedTo(inter);
			inter.addLock(link.lockImpl);
		}

		@Override
	    public void run(Interval current) {
	        transform.transform(link);
	        if(link.next != null) {
	        	Interval next = mapWalk(current.getParent(), link.next, transform);
	        	Intervals.addHb(next.getStart(), current.getEnd());
            }
	    } 
		
		public String toString() {
			return format("MapWalk(link=%s)", link.data);
		}
	}
	
	class DoubleTransform implements Transform {
		@Override
		public void transform(Link l) {
			l.data *= 2;
		}		
	}
	
	Link buildList(int... data) {
		Link list = null;
		for(int i = data.length - 1; i >= 0; i--)
			list = new Link(data[i], list);
		return list;
	}
	
	public @Test void testDouble() {
		final Link list = buildList(5, 10, 25);
		inline(new AbstractTask() {
			public void run(Interval subinterval) {
				mapWalk(subinterval, list, new DoubleTransform());		
			}
		});
		Link expList = buildList(10, 20, 50);
		Assert.assertEquals(expList, list);
	}
}
