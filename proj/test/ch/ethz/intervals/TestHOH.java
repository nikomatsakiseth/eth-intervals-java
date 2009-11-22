package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.blockingInterval;
import static ch.ethz.intervals.Intervals.intervalDuring;
import static ch.ethz.intervals.Intervals.intervalWithBound;
import static java.lang.String.format;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.params.Parent;
import ch.ethz.intervals.quals.Effects;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.New;
import ch.ethz.intervals.quals.ObjectParameter;

@ObjectParameter
@interface HOHList {
    public String value() default "HOHList";
}

public class TestHOH {
    
	@HOHList
	static class Link 
	extends /*@Identity("HOHList")*/ Object {
		@New final @Parent("HOHList") Guard guard;
		@GuardedBy("part") int data;
		@GuardedBy("part") Link next;
		
		public Link(int data, Link next) {
			this.guard = Guards.newGuard();
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
		@Effects("Ex(l.guard)")
		void transform(Link l);
	}

	class MapWalk implements Task<Void> {
	    final Link link;
	    final Transform transform;

		public MapWalk(Link link, Transform transform) {
			this.link = link;
			this.transform = transform;
		}

		@Override @Effects("(mthd-):Ex(link.guard)")
	    public Void run(Interval<Void> current) {
	        transform.transform(link);
	        if(link.next != null) {
	        	intervalWithBound(current.bound())
		        .exclusiveLock(link.next.guard)
		        .startBefore(current.end())
		        .schedule(new MapWalk(link.next, transform));
            }
	        return null;
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
	
	@HOHList("listGuard") Link buildList(int... data) {
		@HOHList("listGuard") Link list = null;
		for(int i = data.length - 1; i >= 0; i--)
			list = new /*@HOHList("listGuard")*/ Link(data[i], list);
		return list;
	}
	
	public @Test void testDouble() {
	    Guard listGuard = Guards.newGuard();
		final @HOHList("listGuard") Link list = buildList(5, 10, 25);
		blockingInterval(new Task<Void>() {
			public Void run(Interval<Void> current) {
				intervalDuring(current).schedule(new MapWalk(list, new DoubleTransform()));		
				return null;
			}
		});
		@HOHList("listGuard") Link expList = buildList(10, 20, 50);
		Assert.assertEquals(expList, list);
	}
}
