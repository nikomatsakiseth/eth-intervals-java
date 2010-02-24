package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.inline;
import static ch.ethz.intervals.util.ChunkList.TEST_EDGE;

import java.util.Collections;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.TestInterval.IncTask;
import ch.ethz.intervals.util.ChunkList;

public class TestErrorPropagation {
	
	static class TestException extends RuntimeException { 
		
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
		try {
			Intervals.inline(new VoidInlineTask() {
				public void run(Interval subinterval) {
					new ThrowExceptionTask(subinterval);
				}
			});
			
			Assert.fail("No exception thrown!");
		} catch (RethrownException e) {
			Assert.assertTrue("Not subtype: "+e.getCause(), e.getCause() instanceof TestException);
		}				
	}
	
	/**
	 * Tests that a catch method can return a new set to substitute
	 * a different exception.
	 */
	@Test public void catchMethodSubstitutesADifferentException() {
		try {
			Intervals.inline(new VoidInlineTask() {
				public void run(Interval subinterval) {
					new ThrowExceptionTask(subinterval) {
	
						@Override
						protected Set<? extends Throwable> catchErrors(
								Set<Throwable> errors) 
						{
							Assert.assertEquals(1, errors.size());
							Throwable t = errors.iterator().next();
							Assert.assertTrue("Not subtype: "+t, t instanceof TestException);
							return Collections.singleton(new UnsupportedOperationException());
						}
						
					};
				}
			});
			
			Assert.fail("No exception thrown!");
		} catch (RethrownException e) {
			Assert.assertTrue("Not subtype: "+e.getCause(), e.getCause() instanceof UnsupportedOperationException);
		}				
	}
	
	/**
	 * Tests that returning an empty set causes no exceptions to be thrown.
	 */
	@Test public void catchMethodReturnsEmptySet() {
		Intervals.inline(new VoidInlineTask() {
			public void run(Interval subinterval) {
				new ThrowExceptionTask(subinterval) {
						@Override
					protected Set<? extends Throwable> catchErrors(
							Set<Throwable> errors) 
					{
						Assert.assertEquals(1, errors.size());
						Throwable t = errors.iterator().next();
						Assert.assertTrue("Not subtype: "+t, t instanceof TestException);
						return Collections.emptySet();
					}
					
				};
			}
		});					
	}
	
	/**
	 * Tests that returning an empty set causes no exceptions to be thrown.
	 */
	@Test public void uncaughtExceptionsPreventSuccessorsFromExecuting() {
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new VoidInlineTask() {
				public void run(Interval subinterval) {
					Interval err = new ThrowExceptionTask(subinterval);
					Interval inc = new IncTask(subinterval, "inc", integer);
					Intervals.addHb(err, inc);
				}
			});
			Assert.fail("Exception not thrown");
		} catch (RethrownException e) {
			Assert.assertEquals("inc task executed", 0, integer.get());
		}
	}
	
	
	/**
	 * Tests that returning an empty set causes no exceptions to be thrown.
	 */
	@Test public void addingChildrenAfterRunMethodFailsIsOk() {
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new VoidInlineTask() {
				public void run(Interval subinterval) {
					final Interval thr = new ThrowExceptionTask(subinterval);					
					
					new Interval(subinterval, "add") {
						{
							thr.start.addEdgeAndAdjust(start, TEST_EDGE);
							Intervals.addHb(end, thr.end); 
						}
						@Override protected void run() {
							// At this stage, thr has started but not ended:
							assert thr.start.didOccur();
							assert !thr.end.didOccur();
							
							// Insert a new child (should never execute):
							new IncTask(thr, "skipped", integer);
						}
					};
				}
			});
			Assert.fail("Exception not thrown");
		} catch (RethrownException e) {
			Assert.assertEquals("inc task executed", 0, integer.get());
		}
	}
	
	private boolean containsSubtypeOf(Iterable<? extends Object> coll, Class<?> cls) {
		for(Object o : coll) {
			if(cls.isInstance(o))
				return true;
		}
		return false;
	}
	
	/**
	 * Tests that returning an empty set causes no exceptions to be thrown.
	 */
	@Test public void addingChildrenDuringCatchErrorsIsUncool() {
		try {
			Intervals.inline(new VoidInlineTask() {
				public void run(Interval subinterval) {
					new ThrowExceptionTask(subinterval) {
						@Override protected Set<? extends Throwable> catchErrors(Set<Throwable> errors) {
							new EmptyInterval(this, "willCauseAnError");
							return null;
						}
					};
				}
			});
			Assert.fail("Exception not thrown");
		} catch (RethrownException e) {
			Assert.assertEquals(2, e.allErrors().size()); // both the original exc and new exc are carried over
			Assert.assertTrue(containsSubtypeOf(e.allErrors(), TestException.class)); // original exc
			Assert.assertTrue(containsSubtypeOf(e.allErrors(), IntervalException.ParPhaseCompleted.class)); // new exc
		}
	}
	
	/**
	 * Check that when an error propagates from a pred of the
	 * end point, the interval is not cancelled but its successors
	 * are.
	 */
	@Test public void predecessorsOfEndThatDie() {
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new VoidInlineTask() {
				public void run(Interval subinterval) {
					Interval thr = new ThrowExceptionTask(subinterval);
					Interval x = new IncTask(subinterval, "x", integer, 1);
					Interval y = new IncTask(subinterval, "y", integer, 10);
					Interval z = new IncTask(subinterval, "z", integer, 100);
					
					Intervals.addHb(x, z);
					Intervals.addHb(x.start, thr.start);
					Intervals.addHb(thr.end, x.end);
				}
			});
			Assert.fail("Exception not thrown");
		} catch (RethrownException e) {
			Assert.assertEquals(1, e.allErrors().size()); // both the original exc and new exc are carried over
			Assert.assertTrue(containsSubtypeOf(e.allErrors(), TestException.class)); // original exc
			Assert.assertEquals(11, integer.get()); // x and y ran, but not z 
		}
	}
	
	/**
	 * 
	 */
	@Test public void predecessorsOfSubintervalEndThatDie() {
		
		//
		//             |---x---| --> |--z--|
		// |--thr--| --> |-y-|
		
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new VoidInlineTask() {
				public void run(Interval subinterval) {
					Interval thr = new ThrowExceptionTask(subinterval);
					Interval x = new IncTask(subinterval, "x", integer, 1);
					Interval y = new IncTask(x, "y", integer, 10);
					Interval z = new IncTask(subinterval, "z", integer, 100);
					
					Intervals.addHb(thr, y);
					Intervals.addHb(x, z);
				}
			});
			Assert.fail("Exception not thrown");
		} catch (RethrownException e) {
			Assert.assertEquals(1, e.allErrors().size()); // both the original exc and new exc are carried over
			Assert.assertTrue(containsSubtypeOf(e.allErrors(), TestException.class)); // original exc
			Assert.assertEquals(1, integer.get()); // x ran, but not y or z 
		}
	}
	
	/**
	 * Check that when an interval has both internal errors
	 * and errors that propagate to its end point, both sets 
	 * of errors reach the parent, and catchErrors() is called.
	 */
	@Test public void predecessorsOfEndThatDieAndInternalErrorsToo() {
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new VoidInlineTask() {
				public void run(Interval subinterval) {
					Interval thr = new ThrowExceptionTask(subinterval);
					Interval a = new ThrowExceptionTask(subinterval) {
						@Override protected Set<? extends Throwable> catchErrors(
								Set<Throwable> errors) 
						{
							return Collections.singleton(new UnsupportedOperationException());
						}						
					};
					Interval b = new IncTask(subinterval, "b", integer, 10);
					
					Intervals.addHb(a, b);
					Intervals.addHb(a.start, thr.start);
					Intervals.addHb(thr.end, a.end);
				}
			});
			Assert.fail("Exception not thrown");
		} catch (RethrownException e) {
			Assert.assertEquals(2, e.allErrors().size()); // both the original exc and new exc are carried over
			Assert.assertTrue(containsSubtypeOf(e.allErrors(), TestException.class)); // exc from thr
			Assert.assertTrue(containsSubtypeOf(e.allErrors(), UnsupportedOperationException.class)); //  exc from a
			Assert.assertEquals(0, integer.get()); // b did not run (doubly so :) 
		}
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
					Intervals.inline(new VoidInlineTask() {
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
		Intervals.inline(new VoidInlineTask() {			
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
					Intervals.inline(new VoidInlineTask() {
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
	
	@Test(expected=NotInRootIntervalException.class) 
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
	
	@Test public void multipleExceptionsCollected() {
		class TestHarness {
			public void test(final int length) {
				try {
					Intervals.inline(new VoidInlineTask() {
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
		

	@Test
	public void parentIntervalsWithUnexecutedChildrenCancelSafely() {
		try {
			Intervals.inline(new VoidInlineTask() {				
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
			inline(new VoidInlineTask() {
				public void run(final Interval subinterval) {	
					new IncTask(subinterval, "inc", i);
					Interval b = new EmptyInterval(subinterval, "b");
					Intervals.addHb(b.end, a.start);
				}			
			});
			
			Assert.fail("NoEdgeException never thrown");
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof EdgeNeededException);
		}
		
		// The increment task should not execute, because its parent died:
		Assert.assertEquals("Increment task executed", 0, i.get());
	}

	@Test 
	public void cycleErrorsLeaveSchedulerInStableState() {
		final AtomicInteger i = new AtomicInteger();
		
		try {
			inline(new VoidInlineTask() {
				public void run(final Interval subinterval) {
					new IncTask(subinterval, "inc", i);
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
		
		Assert.assertEquals("Increment task executed", 0, i.get());	
	}
	
	@Test
	public void raceCycle1() {
		assert Intervals.SAFETY_CHECKS; // or else this test is not so useful.
		final AtomicInteger cnt = new AtomicInteger();
		inline(new VoidInlineTask() {			
			public void run(final Interval subinterval) {
				Interval a = new IncTask(subinterval, "a", cnt);
				Interval b = new IncTask(subinterval, "b", cnt);
				Interval c = new IncTask(subinterval, "c", cnt);
				Interval d = new IncTask(subinterval, "d", cnt);
				
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
		inline(new VoidInlineTask() {			
			@Override public String toString() { return "parent"; }
			@Override public void run(final Interval subinterval) {				
				new Interval(subinterval, "child") {		
					@Override protected void run() {
						Interval b = new IncTask(subinterval, "a", cnt);
						Interval a = new IncTask(subinterval, "b", cnt);
						Interval c = new IncTask(subinterval, "c", cnt);
						Interval d = new IncTask(subinterval, "d", cnt);
						
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
