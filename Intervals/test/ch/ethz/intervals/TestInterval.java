package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.subinterval;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

public class TestInterval {
	
	/** many tests are not guaranteed to fail, so we repeat them 
	 *  many times to stress the scheduler... not the best solution! */
	final int repeat = 1000;
	
	protected static void debug(String fmt, Object... args) {
		//System.err.println(String.format(fmt, args));
	}
	
	class IncTask extends Interval {
		public final AtomicInteger i;

		public IncTask(Dependency dep, AtomicInteger i) {
			super(dep);
			this.i = i;
		}

		@Override
		public void run() {
			i.getAndIncrement();
		}		
	}
	
	class AddTask extends Interval {

		public final List<List<Integer>> list;
		public final List<Integer> id;
		
		public AddTask(Dependency dep, List<List<Integer>> list, Integer... ids) {
			super(dep);
			this.id = Arrays.asList(ids);
			this.list = list;
		}

		@Override
		public void run() 
		{
			debug("%s", toString());
			list.add(id);
		}
		
		@Override
		public String toString() {
			return "AddTask("+id+")";
		}
		
	}
	
	public void checkOrdering(List<List<Integer>> results) {
		List<Integer> current = Arrays.asList(0);
		for (List<Integer> next : results) {
			for (int i = 0; i < current.size(); i++) {
				Assert.assertTrue("Invalid ordering detected: " + current + "/"
						+ next + " in " + results, current.get(i) <= next.get(i));
			}
		}
	}

	@Test public void basic() {
		for (int i = 0; i < repeat; i++) {
			final List<List<Integer>> list = Collections.synchronizedList(new ArrayList<List<Integer>>());
			Intervals.subinterval(new VoidSubinterval() {
				public void run(final Interval parentInterval) {
					Intervals.subinterval(new VoidSubinterval() {
						public void run(final Interval childInterval) {							
							Interval after = new AddTask(parentInterval, list, 2);
							Intervals.addHb(childInterval.end, after.start);
							new AddTask(childInterval, list, 1);
							new AddTask(childInterval, list, 1);
							new AddTask(childInterval, list, 1);							
						}
					});					
				}
			});
			Assert.assertEquals("equal", 4, list.size());
			checkOrdering(list);
		}
	}
	
	/**
	 * Check that if we have a whole bunch of children, they all
	 * complete and execute once.  Don't schedule them right away
	 * to stress test unscheduled list as it gets bigger.
	 */
	@Test public void manyChildren() {
		final int c = 1024;
		final AtomicInteger cnt = new AtomicInteger();
		Intervals.subinterval(new VoidSubinterval() {
			public void run(Interval _) {
				for(int i = 0; i < c; i++)
					new IncTask(Intervals.child(), cnt);
			}			
		});
		Assert.assertEquals(cnt.get(), c);
	}
	
	/**
	 * Check that if we have a whole bunch of children, they all
	 * complete and execute once.  Schedule them right away.
	 */
	@Test public void manyChildrenScheduled() {
		final int c = 1024;
		final AtomicInteger cnt = new AtomicInteger();
		Intervals.subinterval(new VoidSubinterval() {
			public void run(Interval _) {
				for(int i = 0; i < c; i++) {
					new IncTask(Intervals.child(), cnt).schedule();
				}
			}
			
		});
		Assert.assertEquals(cnt.get(), c);
	}
	
	/**
	 * Check that if we schedule a bunch of tasks as children of
	 * a future, not-yet-scheduled interval, everything still works.
	 */
	@Test public void manyDuring() {
		final int c = 1024;
		final AtomicInteger cnt = new AtomicInteger();
		Intervals.subinterval(new VoidSubinterval() {
			public void run(Interval _) {
				Interval future = new EmptyInterval(Intervals.child(), "during");
				for(int i = 0; i < c; i++)
					new IncTask(future, cnt);
			}
			
		});
		Assert.assertEquals(cnt.get(), c);
	}
	
	@Test public void ancestor() {
		/*                    worker
		 *                   /      \
		 *                  d        a
		 *                 / \      / \
		 *                k   n    m   t
		 *                              \
		 *                               s
		 */
		final AtomicInteger successful = new AtomicInteger();
		Intervals.subinterval(new VoidSubinterval() {
			public void run(Interval subinterval) {
				Interval worker = new EmptyInterval(subinterval, "worker");
				Interval d = new EmptyInterval(worker, "d");
				Interval k = new EmptyInterval(d, "k");
				Interval n = new EmptyInterval(d, "n");
				Interval a = new EmptyInterval(worker, "a");
				Interval m = new EmptyInterval(a, "m");
				Interval t = new EmptyInterval(a, "t");
				Interval s = new EmptyInterval(t, "s");
				
				Assert.assertEquals(d.end, d.end.mutualBound(d.end));
				Assert.assertEquals(d.end, k.end.mutualBound(n.end));
				Assert.assertEquals(worker.end, k.end.mutualBound(s.end));
				Assert.assertEquals(worker.end, s.end.mutualBound(k.end));
				Assert.assertEquals(a.end, m.end.mutualBound(s.end));
				Assert.assertEquals(a.end, s.end.mutualBound(m.end));
				
				successful.addAndGet(1); 
			}
		});
		Assert.assertEquals(successful.get(), 1); // just in case an exc. gets swallowed
	}
	
	@SuppressWarnings("serial")
	class TestException extends RuntimeException { 
		
	}
	
	class ThrowExceptionTask extends Interval {
		public ThrowExceptionTask(Dependency dep) {
			super(dep);
		}

		@Override
		public void run() {
			throw new TestException();
		}
	}
	
	/**
	 * Tests that uncaught exceptions propagate up to the parent, etc.
	 */
	@Test public void exceptionPropagatesToParent() {
		class TestHarness {
			public void test() {
				try {
					Intervals.subinterval(new VoidSubinterval() {
						public void run(Interval subinterval) {
							new ThrowExceptionTask(subinterval);
						}
					});
					
					Assert.fail("No exception thrown!");
				} catch (RethrownException e) {
					Assert.assertTrue("Not subtype: "+e.getCause(), e.getCause() instanceof TestException);
				}				
			}
		}
		
		new TestHarness().test();
	}
	
	/**
	 * Uncaught exceptions do not propagate if the MASK_EXC flag is set
	 * and an edge is later added. 
	 */
	@Test public void maskExceptionsOnSpeculativeEdges() {
		// First, run a blocking interval which catches an exception
		// and re-throws it.
		class TestHarness {
			public Interval savedInter;
			public void test() {
				try {
					Intervals.subinterval(new VoidSubinterval() {
						public void run(Interval subinterval) {
							savedInter = subinterval; 
							new ThrowExceptionTask(subinterval);
						}						
					});					
					Assert.fail("No exception thrown!");
				} catch (RethrownException e) {
					Assert.assertTrue("Not subtype: "+e.getCause(), e.getCause() instanceof TestException);
				}		
			}
		}	
		final TestHarness h = new TestHarness();
		h.test();
		
		Assert.assertNotNull("addDependencies was never called", h.savedInter);

		// We now add an edge from the end of the blocking interval,
		// which must have occurred, to the end of this blocking interval.
		// This should not cause an exception to be thrown, even though
		// the end of h.savedInter has an associated exception, because h.savedInter.end
		// (the end of the blocking interval) is set to mask exceptions.
		Intervals.subinterval(new VoidSubinterval() {			
			public void run(Interval subinterval) {
				Intervals.addHb(h.savedInter.end, subinterval.end);
			}
		});
	}
	
	/**
	 * Tests that uncaught exceptions propagate up to the parent, etc.
	 */
	@Test public void exceptionPropagatesToGrandparent() {
		class TestHarness {
			public void test() {
				try {
					Intervals.subinterval(new VoidSubinterval() {
						public void run(Interval _) {
							new Interval(Intervals.child()) {
								@Override protected void run() {
									new ThrowExceptionTask(Intervals.child());
								}								
							};
						}						
					});
					
					Assert.fail("No exception thrown!");
				} catch (RethrownException e) {
					Assert.assertTrue("Not subtype: "+e.getCause(), e.getCause() instanceof TestException);
				}				
			}
		}
		
		new TestHarness().test();
	}

	@Test public void multipleExceptionsCollected() {
		class TestHarness {
			public void test(final int length) {
				try {
					Intervals.subinterval(new VoidSubinterval() {
						public void run(final Interval subinterval) {			
							for(int i = 0; i < length; i++) {
								new ThrowExceptionTask(subinterval);
							}
						}
					});
					Assert.fail("No exception thrown");
				} catch (RethrownException e) {
					Assert.assertEquals("Wrong number of exceptions", length, e.allErrors().size());
				}
			}
		}
		
		for(int length = 5; length < 25; length++) {
			new TestHarness().test(length);
		}
	}	
		
	@Test(expected=EdgeNeededException.class) 
	public void raceConditionInBeforeGeneratesError1() {
		final Interval a = new EmptyInterval(Intervals.root(), "a");
		Intervals.schedule();
		Interval b = new EmptyInterval(Intervals.root(), "b");
		Intervals.addHb(b.end, a.start);
	}
	
	@Test(expected=EdgeNeededException.class) 
	public void raceConditionInBeforeGeneratesError2() {
		final Interval a = new EmptyInterval(Intervals.root(), "a");
		Intervals.schedule();
		Interval b = new EmptyInterval(Intervals.root(), "b");
		Intervals.addHb(b.start, a.end);
	}
	
	@Test(expected=EdgeNeededException.class) 
	public void raceConditionInBeforeGeneratesError3() {
		final Interval a = new EmptyInterval(Intervals.root(), "a");
		Intervals.schedule();
		new EmptyInterval(a, "b");
	}	

	@Test(expected=CycleException.class) 
	public void simpleCycleGeneratesError() {
		final Interval a = new EmptyInterval(Intervals.root(), "a");
		final Interval b = new EmptyInterval(Intervals.root(), "b");
		Intervals.addHb(a.end, b.start);
		Intervals.addHb(b.end, a.start);
	}
	
	@Test(expected=CycleException.class) 
	public void boundToStartGeneratesError() {
		final Interval a = new EmptyInterval(Intervals.root(), "a");
		final Interval b = new EmptyInterval(a, "b");
		Intervals.addHb(a.end, b.start);
	}
	
	@Test(expected=CycleException.class) 
	public void boundToEndGeneratesError() {
		final Interval a = new EmptyInterval(Intervals.root(), "a");
		final Interval b = new EmptyInterval(a, "b");
		Intervals.addHb(a.end, b.end);
	}
	
	@Test
	public void parentIntervalsWithUnexecutedChildrenCancelSafely() {
		try {
			Intervals.subinterval(new VoidSubinterval() {				
				@Override public void run(Interval subinterval) {
					Interval thr = new ThrowExceptionTask(subinterval);
					
					Interval foo = new EmptyInterval(subinterval, "foo");
					new EmptyInterval(foo, "bar");
					
					Intervals.addHb(thr.end, foo.start);
				}
			});
			Assert.fail("Never threw error!");
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof TestException);
		}
	}
	
	@Test 
	public void raceCondErrorsLeaveSchedulerInStableState() {
		final Interval a = new EmptyInterval(Intervals.root(), "a");
		final AtomicInteger i = new AtomicInteger();
		
		try {
			subinterval(new VoidSubinterval() {
				public void run(final Interval subinterval) {	
					new IncTask(subinterval, i);
					Interval b = new EmptyInterval(subinterval, "b");
					Intervals.addHb(b.end, a.start);
				}			
			});
			
			Assert.fail("NoEdgeException never thrown");
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof EdgeNeededException);
		}
		
		Assert.assertEquals("Increment task failed to execute", 1, i.get());
	}

	@Test 
	public void cycleErrorsLeaveSchedulerInStableState() {
		final AtomicInteger i = new AtomicInteger();
		
		try {
			subinterval(new VoidSubinterval() {
				public void run(final Interval subinterval) {
					new IncTask(subinterval, i);
					Interval a = new EmptyInterval(subinterval, "a");
					Interval b = new EmptyInterval(subinterval, "b");
					Intervals.addHb(a.end, b.start);
					Intervals.addHb(b.end, a.start);
				}			
			});
			
			Assert.fail("NoEdgeException never thrown");
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof CycleException);
		}
		
		Assert.assertEquals("Increment task failed to execute", 1, i.get());	
	}
	
	@Test
	public void raceCycle1() {
		assert Intervals.SAFETY_CHECKS; // or else this test is not so useful.
		final AtomicInteger cnt = new AtomicInteger();
		subinterval(new VoidSubinterval() {			
			public void run(final Interval subinterval) {
				Interval a = new IncTask(subinterval, cnt);
				Interval b = new IncTask(subinterval, cnt);
				Interval c = new IncTask(subinterval, cnt);
				Interval d = new IncTask(subinterval, cnt);
				
				Intervals.addHb(a.end, c.start);
				Intervals.addHb(d.end, b.start);
				
				Intervals.optimisticallyAddEdge(b.end, a.start);
				Assert.assertFalse("hb observed before final", b.end.hb(a.start));
				try {
					Intervals.addHb(c.end, d.start);
					Assert.fail("No cycle exception");
				} catch(CycleException e) {					
				}
				Intervals.checkForCycleAndRecover(b.end, a.start);
				Assert.assertTrue("hb not observed after final", b.end.hb(a.start));
			}
		});
		Assert.assertEquals("Not all subintervals executed!", 4, cnt.get());
	}

	@Test
	public void raceCycle2() {
		assert Intervals.SAFETY_CHECKS; // or else this test is not so useful.
		final AtomicInteger cnt = new AtomicInteger();
		subinterval("parent", new VoidSubinterval() {			
			public void run(final Interval subinterval) {				
				new Interval(subinterval, "child") {		
					@Override protected void run() {
						Interval a = new IncTask(subinterval, cnt);
						Interval b = new IncTask(subinterval, cnt);
						Interval c = new IncTask(subinterval, cnt);
						Interval d = new IncTask(subinterval, cnt);
						
						Intervals.addHb(a.end, c.start);
						Intervals.addHb(d.end, b.start);
						
						Intervals.optimisticallyAddEdge(b.end, a.start);
						Assert.assertFalse("hb observed before final", b.end.hb(a.start));
						try {
							Intervals.addHb(c.end, d.start);
							Assert.fail("No cycle exception");
						} catch(CycleException e) {					
						}

						b.schedule();
						d.schedule();
						
						// Have to resort to this wicked internal APIs
						// in order to force the scenario where we started
						// adding an edge b->a and b had not occurred, 
						// found a cycle, but then b occurred before we 
						// could undo the cycle.  This could only happen in practice
						// if another thread was simultaneously adding an edge,
						// we both found the cycle, and then they fixed theirs, allowing
						// the scheduler to proceed, before we could fix ours.
						b.end.join();
						
						// Note: manually invoking this method doesn't cause a CycleException
						Intervals.recoverFromCycle(b.end, a.start);				
						Assert.assertFalse("hb observed after recoverFromCycle", b.end.hb(a.start));
					}
				};
			}
		});
		Assert.assertEquals("Not all subintervals executed!", 4, cnt.get());
	}
}
