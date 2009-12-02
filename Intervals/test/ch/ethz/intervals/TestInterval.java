package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.blockingInterval;
import static ch.ethz.intervals.Intervals.emptyTask;
import static ch.ethz.intervals.Intervals.interval;
import static ch.ethz.intervals.Intervals.intervalDuring;
import static ch.ethz.intervals.Intervals.intervalWithBound;

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
	
	class IncTask extends AbstractTask {
		public final AtomicInteger i;

		public IncTask(AtomicInteger i) {
			this.i = i;
		}

		@Override
		public void run(Point _) {
			i.getAndIncrement();
		}		
	}
	
	class AddTask extends AbstractTask {

		public final List<List<Integer>> list;
		public final List<Integer> id;
		
		public AddTask(List<List<Integer>> list, Integer... ids) {
			this.id = Arrays.asList(ids);
			this.list = list;
		}

		public void run(Point _) {
			debug("%s", toString());
			list.add(id);
		}
		
		@Override
		public String toString() {
			return "AddTask id="+id;
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
			Intervals.blockingInterval(new AbstractTask() {
				public void run(final Point parentEnd) {
					
					Intervals.blockingInterval(new AbstractTask() {
						public void run(final Point childEnd) {
							
							Interval after = intervalWithBound(parentEnd, new AddTask(list, 2));
							Intervals.addHb(childEnd, after.start());
							intervalWithBound(childEnd, new AddTask(list, 1));
							intervalWithBound(childEnd, new AddTask(list, 1));
							intervalWithBound(childEnd, new AddTask(list, 1));
							
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
		Intervals.blockingInterval(new AbstractTask() {
			
			@Override
			public void run(Point currentEnd) {
				for(int i = 0; i < c; i++)
					Intervals.childInterval(new IncTask(cnt));
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
		Intervals.blockingInterval(new AbstractTask() {
			
			@Override
			public void run(Point currentEnd) {
				for(int i = 0; i < c; i++) {
					Intervals.childInterval(new IncTask(cnt));
					Intervals.schedule();
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
		Intervals.blockingInterval(new AbstractTask() {
			
			@Override
			public void run(Point currentEnd) {
				Interval future = Intervals.childInterval(Intervals.emptyTask);
				for(int i = 0; i < c; i++)
					Intervals.intervalDuring(future, new IncTask(cnt));
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
		Intervals.blockingInterval(new AbstractTask() {
			@Override
			public void run(Point end) {
				IntervalImpl worker = (IntervalImpl) intervalWithBound(end, emptyTask);
				IntervalImpl d = (IntervalImpl) intervalDuring(worker, emptyTask);
				IntervalImpl k = (IntervalImpl) intervalDuring(d, emptyTask);
				IntervalImpl n = (IntervalImpl) intervalDuring(d, emptyTask);
				IntervalImpl a = (IntervalImpl) intervalDuring(worker, emptyTask);
				IntervalImpl m = (IntervalImpl) intervalDuring(a, emptyTask);
				IntervalImpl t = (IntervalImpl) intervalDuring(a, emptyTask);
				IntervalImpl s = (IntervalImpl) intervalDuring(t, emptyTask);
				
				Assert.assertEquals(d.end, d.end.mutualBound(d.end));
				Assert.assertEquals(d.end, k.end.mutualBound(n.end));
				Assert.assertEquals(worker.end(), k.end.mutualBound(s.end));
				Assert.assertEquals(worker.end(), s.end.mutualBound(k.end));
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
	
	class ThrowExceptionTask extends AbstractTask {
		@Override
		public void run(Point currentEnd) {
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
					Intervals.blockingInterval(new AbstractTask() {

						@Override
						public void run(Point currentEnd) {
							intervalWithBound(currentEnd, new ThrowExceptionTask());
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
	 * Tests that uncaught exceptions propagate up to the parent, etc.
	 */
	@Test public void exceptionPropagatesToGrandparent() {
		class TestHarness {
			public void test() {
				try {
					Intervals.blockingInterval(new AbstractTask() {

						@Override
						public void run(Point currentEnd) {
							intervalWithBound(
									currentEnd, 
									new SubintervalTask(new ThrowExceptionTask()));
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
	 * Tests that uncaught exceptions propagate up to the parent, etc.
	 */
	@Test public void exceptionPropagatesToSuccessors() {
		final AtomicInteger ctr = new AtomicInteger();
		final IncTask incTask = new IncTask(ctr);
		
		class TestHarness {
			public void test(final int length) {
				Intervals.blockingInterval(new AbstractTask() {
					
					@Override
					public void run(Point outerEnd) {
						try {
							Intervals.blockingInterval(new AbstractTask() {
								@Override
								public void run(Point currentEnd) {
									Interval link0 = Intervals.interval(new ThrowExceptionTask());
									Interval linkN = link0;
									for(int i = 0; i < length; i++) {
										linkN = Intervals.interval(incTask);
										Intervals.addHb(link0.end(), linkN.start());
									}

									Intervals.addHb(linkN.end(), currentEnd);
								}
							});
							
							Assert.fail("No exception thrown!");
						} catch (RethrownException e) {
							Assert.assertTrue("Not subtype: "+e.getCause(), e.getCause() instanceof TestException);
						}
					}
				});
				
				// The incTasks should be cancelled!				
				Assert.assertEquals(0, ctr.get());
			}
		}
		
		for(int length = 5; length < 25; length++) {
			new TestHarness().test(length);
		}
	}
	
	@Test public void multipleExceptionsCollected() {
		class TestHarness {
			public void test(final int length) {
				try {
					Intervals.blockingInterval(new AbstractTask() {
						@Override
						public void run(Point currentEnd) {
							for(int i = 0; i < length; i++) {
								Intervals.childInterval(new ThrowExceptionTask());
							}
						}
					});
					Assert.fail("No exception thrown");
				} catch (RethrownException e) {
					Assert.assertEquals("Wrong number of exceptions", length, e.allErrors.size());
				}
			}
		}
		
		for(int length = 5; length < 25; length++) {
			new TestHarness().test(length);
		}
	}	
	
	// A test of a P2P setup.  
	@Test public void pointToPoint() {
		for (int iter = 0; iter < repeat; iter++)
			_pointToPoint();
	}
	
	public void _pointToPoint() {
		// includes 2 "boundary" columns:
		final int R = 25, C = 25;
		final Point[][] intervals = new Point[2][C+2];
		final int[][] data = new int[R][C];
		final int dataPoints = 45;
			
		// Point-to-point synchronizing cell:
		//
		//   Each Cell represents the processing of one cell.
		//   After it has finished, it creates a new interval
		//   for processing the cell in the same column but
		//   the next row down.  This processing cannot proceed,
		//   however, unless the neighboring cells in the same 
		//   row have finished as well.
		//
		//   To maintain this, we use the two arrays of intervals
		//   shown above.  Intervals for each column of even
		//   rows are in intervals[0][C], and for odd rows intervals[1][C].
		//
		//   Note that whenever a cell in an even row is executing,
		//   it knows that the cells in the previous odd row have
		//   finished and therefore updated the intervals in the
		//   even row.  To create the next interval for the next row,
		//   it is therefore safe to read the neighboring cells.
		//
		//   To deal with the boundary cases, we make the intervals
		//   array 2 entries bigger than C.
		Intervals.blockingInterval(new SetupTask() {
			
			public Point intervalFor(int row, int col) {
				return intervals[row & 1][col + 1];
			}

			public void setIntervalFor(int row, int col, Point iEnd) {
				intervals[row & 1][col + 1] = iEnd;
			}
			
			public void setup(Point phase0End, final Interval parent) {				
				class Cell extends AbstractTask {
					// coordinate of data:
					final int row, col;
					
					// shared counter indicating how many cells to process:
					final AtomicInteger counter;
					
					public Cell(int row, int col, AtomicInteger counter) {
						this.row = row;
						this.col = col;
						this.counter = counter;
					}
					
					@Override
					public void addDependencies(Interval inter) {
						super.addDependencies(inter);
						Intervals.addHb(intervalFor(row - 1, col - 1), inter.start());
						Intervals.addHb(intervalFor(row - 1, col + 0), inter.start());
						Intervals.addHb(intervalFor(row - 1, col + 1), inter.start());
					}

					public void run(Point iEnd) {
						assert intervalFor(row, col) == iEnd : String.format(
								"Invalid interval at (%d,%d): expected %s but found %s", 
								row, col, iEnd, intervalFor(row, col));
						
						int value = counter.getAndIncrement();
						//debug("Cell @ %d,%d: value=%d interval=%s", row, col, value, i);
						
						if (value > dataPoints)
							return;
						
						data[row][col] = value;
						
						// if not maximum row, proceed to next row
						if (row < R) {
							Cell nextCell = new Cell(row+1, col, counter);
							Interval nextInterval = intervalDuring(parent, nextCell);
							setIntervalFor(row + 1, col, nextInterval.end()); 
						}
					}
				}
				
				final AtomicInteger counter = new AtomicInteger(0);
				for (int c = 0; c < C; c++)
					setIntervalFor(0, c, intervalDuring(parent, new Cell(0, c, counter)).end());
			}			
			
		});
		
		// Dump out data
		for (int r = 0; r < R; r++) {
			StringBuffer sb = new StringBuffer();
			sb.append(String.format("%2d:", r));
			for (int c = 0; c < C; c++) {
				sb.append(String.format(" %2d", data[r][c]));
			}
			debug("%s", sb);
			//System.err.println(sb.toString());
		}				
		
		// Validate properties
		boolean[] found = new boolean[dataPoints+1];
		for (int r = 0; r < R; r++) {
			for (int c = 0; c < C; c++) {
				Assert.assertTrue(
						"Invalid data at "+r+","+c, 
						data[r][c] >= 0 && data[r][c] <= dataPoints);
				
				found[data[r][c]] = true;

				if (r > 0) {
					Assert.assertTrue(
							"Invalid north neighbor at "+r+","+c, 
							data[r][c] == 0 || data[r][c] > data[r-1][c]);
					if (c > 0)
						Assert.assertTrue(
								"Invalid northwest neighbor at "+r+","+c, 
								data[r][c] == 0 || data[r][c] > data[r-1][c-1]);				
					if (c < C-1)
						Assert.assertTrue(
								"Invalid northeast neighbor at "+r+","+c, 
								data[r][c] == 0 || data[r][c] > data[r-1][c+1]);				
				}				
			}
		}
		
		for (int dp = 0; dp <= dataPoints; dp++)
			Assert.assertTrue("No data point "+dp, found[dp]);
	}
	
	@Test(expected=NoEdgeException.class) 
	public void raceConditionInBeforeGeneratesError1() {
		final Interval a = interval(Intervals.emptyTask);
		Intervals.schedule();
		Interval b = interval(Intervals.emptyTask);
		Intervals.addHb(b.end(), a.start());
	}
	
	@Test(expected=NoEdgeException.class) 
	public void raceConditionInBeforeGeneratesError2() {
		final Interval a = interval(Intervals.emptyTask);
		Intervals.schedule();
		Interval b = interval(Intervals.emptyTask);
		Intervals.addHb(b.start(), a.end());
	}
	
	@Test(expected=NoEdgeException.class) 
	public void raceConditionInBeforeGeneratesError3() {
		final Interval a = interval(Intervals.emptyTask);
		Intervals.schedule();
		intervalWithBound(a.start(), Intervals.emptyTask);
	}	

	@Test(expected=CycleException.class) 
	public void simpleCycleGeneratesError() {
		final Interval a = interval(Intervals.emptyTask);
		Intervals.addHb(Intervals.ROOT_END, a.end());
	}
	
	@Test(expected=CycleException.class) 
	public void simpleCycleGeneratesError2() {
		final Interval a = interval(Intervals.emptyTask);
		final Interval b = interval(Intervals.emptyTask);
		Intervals.addHb(a.end(), b.start());
		Intervals.addHb(b.end(), a.start());
	}
	
	@Test 
	public void raceCondErrorsLeaveSchedulerInStableState() {
		final Interval a = interval(Intervals.emptyTask);
		final AtomicInteger i = new AtomicInteger();
		
		try {
			blockingInterval(new AbstractTask() {
				public void run(Point currentEnd) {
					intervalWithBound(currentEnd, new IncTask(i));
					Interval b = intervalWithBound(currentEnd, Intervals.emptyTask);
					Intervals.addHb(b.end(), a.start());
				}			
			});
			
			Assert.fail("NoEdgeException never thrown");
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof NoEdgeException);
		}
		
		Assert.assertEquals("Increment task failed to execute", 1, i.get());
	}

	@Test 
	public void cycleErrorsLeaveSchedulerInStableState() {
		final AtomicInteger i = new AtomicInteger();
		
		try {
			blockingInterval(new AbstractTask() {
				public void run(Point currentEnd) {
					intervalWithBound(currentEnd, new IncTask(i));
					Interval a = intervalWithBound(currentEnd, Intervals.emptyTask);
					Interval b = intervalWithBound(currentEnd, Intervals.emptyTask);
					Intervals.addHb(a.end(), b.start());
					Intervals.addHb(b.end(), a.start());
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
		blockingInterval(new AbstractTask() {			
			@Override
			public void run(Point currentEnd) {
				IncTask task = new IncTask(cnt);
				IntervalImpl a = (IntervalImpl) Intervals.childInterval(task);
				IntervalImpl b = (IntervalImpl) Intervals.childInterval(task);
				IntervalImpl c = (IntervalImpl) Intervals.childInterval(task);
				IntervalImpl d = (IntervalImpl) Intervals.childInterval(task);
				
				Intervals.addHb(a.end, c.start);
				Intervals.addHb(d.end, b.start);
				
				int adjust = Intervals.optimisticallyAddEdge(b.end, a.start);
				Assert.assertFalse("hb observed before final", b.end.hb(a.start));
				try {
					Intervals.addHb(c.end, d.start);
					Assert.fail("No cycle exception");
				} catch(CycleException e) {					
				}
				Intervals.checkForCycleAndRecover(b.end, a.start, adjust);
				Assert.assertTrue("hb not observed after final", b.end.hb(a.start));
			}
		});
		Assert.assertEquals("Not all subintervals executed!", 4, cnt.get());
	}

	@Test
	public void raceCycle2() {
		assert Intervals.SAFETY_CHECKS; // or else this test is not so useful.
		final AtomicInteger cnt = new AtomicInteger();
		blockingInterval(new AbstractTask() {			
			@Override
			public void run(Point currentEnd) {
				IncTask task = new IncTask(cnt);
				IntervalImpl a = (IntervalImpl) Intervals.childInterval(task);
				IntervalImpl b = (IntervalImpl) Intervals.childInterval(task);
				IntervalImpl c = (IntervalImpl) Intervals.childInterval(task);
				IntervalImpl d = (IntervalImpl) Intervals.childInterval(task);
				
				Intervals.addHb(a.end, c.start);
				Intervals.addHb(d.end, b.start);
				
				int adjust = Intervals.optimisticallyAddEdge(b.end, a.start);
				Assert.assertFalse("hb observed before final", b.end.hb(a.start));
				try {
					Intervals.addHb(c.end, d.start);
					Assert.fail("No cycle exception");
				} catch(CycleException e) {					
				}

				// Have to resort to some wicked internal APIs
				// in order to force the scenario where we started
				// adding an edge b->a and b had not occurred, 
				// found a cycle, but then b occurred before we 
				// could undo the cycle.  This could only happen in practice
				// if another thread was simultaneously adding an edge,
				// we both found the cycle, and then they fixed theirs, allowing
				// the scheduler to proceed, before we could fix ours.
				Current current = Current.get();
				current.schedule(b);
				current.schedule(d);				
				b.end.join();
				
				try {
					Intervals.recoverFromCycle(b.end, a.start, adjust);
					Assert.fail("No cycle exception");
				} catch(CycleException e) {					
				}
				
				Assert.assertFalse("hb observed after recoverFromCycle", b.end.hb(a.start));
			}
		});
		Assert.assertEquals("Not all subintervals executed!", 4, cnt.get());
	}
}
