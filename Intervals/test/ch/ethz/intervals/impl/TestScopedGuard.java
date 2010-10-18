package ch.ethz.intervals.impl;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import ch.ethz.intervals.AsyncInterval;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.guard.ScopedGuard;
import ch.ethz.intervals.task.AbstractTask;

public class TestScopedGuard extends TestUtil {

	@Test public void testReadableDuring() {
		final Lock lock = Intervals.lock("lock");
		final AtomicInteger value = new AtomicInteger();
		Intervals.inline(new AbstractTask("Root") {
			@Override public void run(final Interval root) {
				class Foo {
					final AsyncInterval foo; 
					final ScopedGuard sg;
					
					public Foo() {
						foo = root.newAsyncChild(new AbstractTask("Foo") {
							@Override
							public void run(Interval current) {
								value.addAndGet(1);
								Intervals.checkReadable(sg);
								value.addAndGet(10);
							}
						});
						
						sg = new ScopedGuard(foo, lock);
						
						foo.schedule();
					}
				}
				
				new Foo();
			}
		});
	}
	
}
