package ch.ethz.intervals.impl;

import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.Condition;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;
import ch.ethz.intervals.guard.DynamicGuard;
import ch.ethz.intervals.impl.TestAtMostOne.AtMostOneGuard.AlreadyClaimed;
import ch.ethz.intervals.task.AbstractTask;

public class TestAtMostOne {
	
	/** 
	 * A dynamic guard which permits at most one interval to
	 * write but which does not know that interval in advance.
	 */
	static class AtMostOneGuard implements DynamicGuard {
		
		@SuppressWarnings("serial")
		static class AlreadyClaimed extends RuntimeException {
			
			public final RoInterval byInterval;

			public AlreadyClaimed(RoInterval byInterval) {
				this.byInterval = byInterval;
			}
			
			@Override
			public String toString() { 
				return "Already claimed by " + byInterval;
			}
			
		}
		
		AtomicReference<RoInterval> ref = new AtomicReference<RoInterval>();
		
		@Override
		public Condition condReadableBy() {
			return condWritableBy();
		}

		@Override
		public Condition condWritableBy() {
			RoInterval w = ref.get();
			if(w == null) 
				return Condition.FALSE;
			return w.condWritableBy();
		}

		@Override
		public Condition condFinal() {
			return Condition.FALSE;
		}

		@Override
		public void makeReadableBy(RoPoint mr, RoInterval current) {
			makeWritableBy(mr, current);
		}

		@Override
		public void makeWritableBy(RoPoint mr, RoInterval current) {
			if(ref.compareAndSet(null, current))
				return; // claimed for inter
			
			RoInterval writer = ref.get();
			if(writer == current)
				return; // previously claimed for inter
			
			throw new AlreadyClaimed(writer);
		}

		@Override
		public void makeFinal(RoPoint mr, RoInterval current) {
			throw new IntervalException.NeverFinal(this);
		}

	}
	
	@Test public void testAtMostOne() {
		final AtMostOneGuard guard = new AtMostOneGuard();
		final AtomicInteger integer = new AtomicInteger(0); // "guarded by" guard
		final int max = 10;
		
		Intervals.inline(new AbstractTask("testAtMostOne") {
			class IncTask extends AbstractTask {
				@Override
				public void run(Interval current) {
					Intervals.makeWritable(guard);
					Intervals.checkWritable(guard);
					integer.incrementAndGet();
				}
			}

			@Override
			public void run(Interval current) {
				for(int i = 0; i < max; i++) {
					current.newAsyncChild(new IncTask());
				}
			}

			@Override
			public Set<? extends Throwable> catchErrors(Set<Throwable> errors) {
				// Expected: every interval but one fails with an AlreadyClaimed
				// exception.
				if(errors.size() == max - 1) {
					Iterator<Throwable> iter = errors.iterator();
					try {
						Throwable first = iter.next();
						RoInterval succ = ((AlreadyClaimed)first).byInterval;
						for(Throwable t : errors) {
							if(((AlreadyClaimed)t).byInterval != succ)
								return errors;
						}
						return null; // squash exc. if we saw expected errors.
					} catch (ClassCastException e) {
						return errors;
					}
				}
				return errors;
			}
		});
		
		// Expected: only one succeeds.
		Assert.assertEquals(1, integer.intValue());
	}

}
