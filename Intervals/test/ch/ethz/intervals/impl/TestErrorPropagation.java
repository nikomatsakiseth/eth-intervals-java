package ch.ethz.intervals.impl;

import static ch.ethz.intervals.Intervals.inline;
import static ch.ethz.intervals.util.ChunkList.TEST_EDGE;

import java.util.Collections;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.CycleException;
import ch.ethz.intervals.EdgeNeededException;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.impl.TestInterval.IncTask;
import ch.ethz.intervals.mirror.AsyncInterval;
import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;

public class TestErrorPropagation extends Util {
	
	@SuppressWarnings("serial")
	static class TestException extends RuntimeException { 
		
	}
	
	class ThrowExceptionTask extends AbstractTask {
		
		ThrowExceptionTask() {
			super();
		}

		ThrowExceptionTask(String name) {
			super(name);
		}

		@Override
		public void run(Interval current) {
			throw new TestException();
		}
		
	}
	
	/**
	 * Tests that uncaught exceptions propagate up to the parent, etc.
	 */
	@Test public void exceptionPropagatesToParent() {
		try {
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					subinterval.newAsyncChild(new ThrowExceptionTask());
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
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					subinterval.newAsyncChild(new ThrowExceptionTask() {
						@Override
						public Set<? extends Throwable> catchErrors(
								Set<Throwable> errors) 
						{
							Assert.assertEquals(1, errors.size());
							Throwable t = errors.iterator().next();
							Assert.assertTrue("Not subtype: "+t, t instanceof TestException);
							return Collections.singleton(new UnsupportedOperationException());
						}
						
					});
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
		Intervals.inline(new AbstractTask() {
			public void run(Interval subinterval) {
				subinterval.newAsyncChild(new ThrowExceptionTask() {
					@Override
					public Set<? extends Throwable> catchErrors(
							Set<Throwable> errors) 
					{
						Assert.assertEquals(1, errors.size());
						Throwable t = errors.iterator().next();
						Assert.assertTrue("Not subtype: "+t, t instanceof TestException);
						return Collections.emptySet();
					}
				});
			}
		});					
	}
	
	/**
	 * Tests that returning an empty set causes no exceptions to be thrown.
	 */
	@Test public void uncaughtExceptionsPreventSuccessorsFromExecuting() {
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					Interval err = subinterval.newAsyncChild(new ThrowExceptionTask());
					Interval inc = subinterval.newAsyncChild(new IncTask("inc", integer));
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
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					final Interval thr = subinterval.newAsyncChild(new ThrowExceptionTask());					
					
					Interval add = subinterval.newAsyncChild(new AbstractTask("add") {
						@Override
						public void run(Interval current) throws Exception {
							// At this stage, thr has started but not ended:
							assert impl(thr.getStart()).didOccur();
							assert !impl(thr.getEnd()).didOccur();
							
							// Insert a new child (should never execute):
							thr.newAsyncChild(new IncTask("skipped", integer));
						}
					});
					impl(thr.getStart()).addEdgeAndAdjust(impl(add.getStart()), TEST_EDGE);
					Intervals.addHb(add.getEnd(), thr.getEnd());
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
	 * Tests that adding children during to an interval during its
	 * catch errors phase results in another error.  Also tests 
	 * that exceptions thrown during catchErrors are added to
	 * the result set.
	 */
	@Test public void addingChildrenDuringCatchErrorsIsUncool() {
		try {
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					subinterval.newAsyncChild(new ThrowExceptionTask() {
						Interval me;
						
						@Override public void run(Interval current) {
							me = current;
							super.run(current);
						}
						
						@Override public Set<? extends Throwable> catchErrors(Set<Throwable> errors) {
							me.newAsyncChild(new EmptyTask("willCauseAnError"));
							return null;
						}
					});
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
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					Interval thr = subinterval.newAsyncChild(new ThrowExceptionTask());
					Interval x = subinterval.newAsyncChild(new IncTask("x", integer, 1));
					Interval y = subinterval.newAsyncChild(new IncTask("y", integer, 10));
					Interval z = subinterval.newAsyncChild(new IncTask("z", integer, 100));
					
					Intervals.addHb(x, z);
					Intervals.addHb(x.getStart(), thr.getStart());
					Intervals.addHb(thr.getEnd(), x.getEnd());
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
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					Interval thr = subinterval.newAsyncChild(new ThrowExceptionTask());
					Interval x = subinterval.newAsyncChild(new IncTask("x", integer, 1));
					Interval y = x.newAsyncChild(new IncTask("y", integer, 10));
					Interval z = subinterval.newAsyncChild(new IncTask("z", integer, 100));
					
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
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					Interval thr = subinterval.newAsyncChild(new ThrowExceptionTask());
					Interval a = subinterval.newAsyncChild(new ThrowExceptionTask() {
						@Override public Set<? extends Throwable> catchErrors(
								Set<Throwable> errors) 
						{
							return Collections.singleton(new UnsupportedOperationException());
						}						
					});
					Interval b = subinterval.newAsyncChild(new IncTask("b", integer, 10));
					
					Intervals.addHb(a, b);
					Intervals.addHb(a.getStart(), thr.getStart());
					Intervals.addHb(thr.getEnd(), a.getEnd());
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
					Intervals.inline(new AbstractTask() {
						public void run(Interval subinterval) {
							savedInter = subinterval; 
							subinterval.newAsyncChild(new ThrowExceptionTask());
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
		Intervals.inline(new AbstractTask() {			
			public void run(Interval subinterval) {
				Intervals.addHb(h.savedInter.getEnd(), subinterval.getEnd());
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
					Intervals.inline(new AbstractTask() {
						public void run(Interval current) {
							current.newAsyncChild(new AbstractTask("child") {
								@Override
								public void run(Interval current) throws Exception {
									current.newAsyncChild(new ThrowExceptionTask());
								}
							});
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
	
	@Test 
	public void raceConditionInBeforeGeneratesError1() {
		expect(new ExpectedErrorTask(EdgeNeededException.class) {
			@Override protected void tryIt(Interval current) {
				final AsyncInterval a = current.newAsyncChild(new EmptyTask("a"));
				a.schedule();
				AsyncInterval b = current.newAsyncChild(new EmptyTask("b"));
				Intervals.addHb(b.getEnd(), a.getStart());				
			}
		});
	}
	
	@Test
	public void raceConditionInBeforeGeneratesError2() {
		expect(new ExpectedErrorTask(EdgeNeededException.class) {
			@Override protected void tryIt(Interval current) {
				final AsyncInterval a = current.newAsyncChild(new EmptyTask("a"));
				a.schedule();
				Interval b = current.newAsyncChild(new EmptyTask("b"));
				Intervals.addHb(b.getStart(), a.getEnd());
			}
		});
	}
	
	@Test
	public void simpleCycleGeneratesError() {
		Assert.assertTrue(Intervals.inline(new ExpectedErrorTask(CycleException.class) {
			@Override protected void tryIt(Interval current) {
				final Interval a = current.newAsyncChild(new EmptyTask("a"));
				final Interval b = current.newAsyncChild(new EmptyTask("b"));
				Intervals.addHb(a.getEnd(), b.getStart());
				Intervals.addHb(b.getEnd(), a.getStart());
			}
		}));
	}
	
	@Test
	public void boundToStartGeneratesError() {
		expect(new ExpectedErrorTask(CycleException.class) {
			@Override protected void tryIt(Interval current) {
				final Interval a = current.newAsyncChild(new EmptyTask("a"));
				final Interval b = a.newAsyncChild(new EmptyTask("b"));
				Intervals.addHb(a, b);
			}
		});
	}
	
	@Test 
	public void boundToEndGeneratesError() {
		expect(new ExpectedErrorTask(CycleException.class) {
			@Override protected void tryIt(Interval current) {
				final Interval a = current.newAsyncChild(new EmptyTask("a"));
				final Interval b = a.newAsyncChild(new EmptyTask("b"));
				Intervals.addHb(a.getEnd(), b.getEnd());
			}
		});
	}
	
	@Test public void multipleExceptionsCollected() {
		class TestHarness {
			public void test(final int length) {
				try {
					Intervals.inline(new AbstractTask() {
						public void run(final Interval subinterval) {			
							for(int i = 0; i < length; i++) {
								subinterval.newAsyncChild(new ThrowExceptionTask());
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
			Intervals.inline(new AbstractTask() {				
				@Override public void run(Interval subinterval) {
					Interval thr = subinterval.newAsyncChild(new ThrowExceptionTask());
					Interval foo = subinterval.newAsyncChild(new EmptyTask("foo"));
					foo.newAsyncChild(new EmptyTask("bar"));
					
					Intervals.addHb(thr.getEnd(), foo.getStart());
				}
			});
			Assert.fail("Never threw error!");
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof TestException);
		}
	}
	
	@Test
	public void raceCycle1() {
		assert Intervals.SAFETY_CHECKS; // or else this test is not so useful.
		final AtomicInteger cnt = new AtomicInteger();
		inline(new AbstractTask() {			
			public void run(final Interval subinterval) {
				Interval a = subinterval.newAsyncChild(new IncTask("a", cnt));
				Interval b = subinterval.newAsyncChild(new IncTask("b", cnt));
				Interval c = subinterval.newAsyncChild(new IncTask("c", cnt));
				Interval d = subinterval.newAsyncChild(new IncTask("d", cnt));
				
				Intervals.addHb(a, c);
				Intervals.addHb(d, b);
				
				impl(b.getEnd()).optimisticallyAddEdge(impl(a.getStart()));
				Assert.assertFalse("hb observed before final", b.getEnd().hb(a.getStart()));
				try {
					Intervals.addHb(c.getEnd(), d.getStart());
					Assert.fail("No cycle exception");
				} catch(CycleException e) {					
				}
				impl(b.getEnd()).checkForCycleAndRecover(impl(a.getStart()));
				Assert.assertTrue("hb not observed after final", b.getEnd().hb(a.getStart()));
			}
		});
		Assert.assertEquals("Not all subintervals executed!", 4, cnt.get());
	}

	@Test
	public void raceCycle2() {
		assert Intervals.SAFETY_CHECKS; // or else this test is not so useful.
		final AtomicInteger cnt = new AtomicInteger();
		inline(new AbstractTask() {			
			@Override public String toString() { return "parent"; }
			@Override public void run(final Interval subinterval) {
				subinterval.newAsyncChild(new AbstractTask("child") {
					
					@Override
					public void run(Interval current) throws Exception {
						AsyncInterval a = subinterval.newAsyncChild(new IncTask("a", cnt));
						AsyncInterval b = subinterval.newAsyncChild(new IncTask("b", cnt));
						AsyncInterval c = subinterval.newAsyncChild(new IncTask("c", cnt));
						AsyncInterval d = subinterval.newAsyncChild(new IncTask("d", cnt));
						
						Intervals.addHb(a, c);
						Intervals.addHb(d, b);
						
						impl(b.getEnd()).optimisticallyAddEdge(impl(a.getStart()));
						Assert.assertFalse("hb observed before final", b.getEnd().hb(a.getStart()));
						try {
							Intervals.addHb(c.getEnd(), d.getStart());
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
						impl(b.getEnd()).join();
						
						// Note: manually invoking this method doesn't cause a CycleException
						impl(b.getEnd()).recoverFromCycle(impl(a.getStart()));				
						Assert.assertFalse("hb observed after recoverFromCycle", b.getEnd().hb(a.getStart()));
					}
				});
			}
		});
		Assert.assertEquals("Not all subintervals executed!", 4, cnt.get());
	}

	/**
	 * Check that when an error propagates from a pred of the
	 * end point, the interval is not cancelled but its successors
	 * are.
	 */
	@Test public void successorsOfStartPoint() {
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new AbstractTask() {
				public void run(Interval subinterval) {
					Interval thr = subinterval.newAsyncChild(new ThrowExceptionTask());
					Interval x = subinterval.newAsyncChild(new IncTask("x", integer, 1));
					Interval y = subinterval.newAsyncChild(new IncTask("y", integer, 10));
					Intervals.addHb(thr.getEnd(), x.getStart());
					Intervals.addHb(x.getStart(), y.getStart());
					Intervals.addHb(y.getEnd(), x.getEnd());
				}
			});
			Assert.fail("Exception not thrown");
		} catch (RethrownException e) {
			Assert.assertEquals(1, e.allErrors().size()); // both the original exc and new exc are carried over
			Assert.assertTrue(containsSubtypeOf(e.allErrors(), TestException.class)); // original exc
			Assert.assertEquals(0, integer.get()); // x and y ran, but not z 
		}
	}

}
