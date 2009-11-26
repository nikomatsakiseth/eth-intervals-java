package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.blockingInterval;
import static ch.ethz.intervals.Intervals.emptyTask;
import static ch.ethz.intervals.Intervals.end;
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
							
							intervalWithBound(parentEnd).startAfter(childEnd).schedule(new AddTask(list, 2));
							intervalWithBound(childEnd).schedule(new AddTask(list, 1));
							intervalWithBound(childEnd).schedule(new AddTask(list, 1));
							intervalWithBound(childEnd).schedule(new AddTask(list, 1));
							
						}
					});
					
				}				
			});
			Assert.assertEquals("equal", 4, list.size());
			checkOrdering(list);
		}
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
		Intervals.blockingInterval(new SetupTask() {
			@Override
			public void setup(Point _, Interval worker) {
				IntervalImpl d = (IntervalImpl) intervalDuring(worker).schedule(emptyTask);
				IntervalImpl k = (IntervalImpl) intervalDuring(d).schedule(emptyTask);
				IntervalImpl n = (IntervalImpl) intervalDuring(d).schedule(emptyTask);
				IntervalImpl a = (IntervalImpl) intervalDuring(worker).schedule(emptyTask);
				IntervalImpl m = (IntervalImpl) intervalDuring(a).schedule(emptyTask);
				IntervalImpl t = (IntervalImpl) intervalDuring(a).schedule(emptyTask);
				IntervalImpl s = (IntervalImpl) intervalDuring(t).schedule(emptyTask);
				
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
	
	/**
	 * Tests that uncaught exceptions propagate up to the parent, etc.
	 */
	@Test public void exceptionPropagatesToParent() {
		class TestHarness {
			public void test(final boolean maskExceptions) {
				try {
					Intervals.blockingInterval(new Task() {

						@Override
						public void run(Point currentEnd) {
							
							intervalWithBound(currentEnd)
							.setMaskExceptions(maskExceptions)
							.schedule(new Task() {

								@Override
								public void run(Point currentEnd) {
									throw new TestException();
								}
								
							});
							
						}
						
					});
					
					if(!maskExceptions)
						Assert.fail("No exception thrown!");
				} catch (RethrownException e) {
					if(!maskExceptions)
						Assert.assertTrue("Not subtype: "+e.getCause(), e.getCause() instanceof TestException);
					else
						Assert.fail("Mask is true, but exception thrown: " + e);
				}				
			}
		}
		
		new TestHarness().test(false);
	}
	
	/**
	 * Tests that uncaught exceptions propagate up to the parent, etc.
	 */
	@Test public void exceptionPropagatesToGrandparent() {
		class TestHarness {
			public void test(final boolean maskExceptions) {
				try {
					Intervals.blockingInterval(new Task() {

						@Override
						public void run(Point currentEnd) {
							
							intervalWithBound(currentEnd)
							.setMaskExceptions(maskExceptions)
							.schedule(new SubintervalTask(new Task() {

								@Override
								public void run(Point currentEnd) {
									throw new TestException();
								}
								
							}));
						}
						
					});
					
					if(!maskExceptions)
						Assert.fail("No exception thrown!");
				} catch (RethrownException e) {
					if(!maskExceptions)
						Assert.assertTrue("Not subtype: "+e.getCause(), e.getCause() instanceof TestException);
					else
						Assert.fail("Mask is true, but exception thrown: " + e);
				}				
			}
		}
		
		new TestHarness().test(false);
	}
	
//	@Test public void phased() {
//		// Repeat 10 times, hoping to catch the scheduler in a screw up!
//		
//		for (int i = 0; i < repeat; i++) {
//			final List<List<Integer>> list = Collections.synchronizedList(new ArrayList<List<Integer>>());
//			
//			class Subinterval extends AbstractTask {
//				int phase;
//
//				public Subinterval(int phase) {
//					this.phase = phase;
//				}								
//				
//				public Void run(Interval _) {
//					Intervals.forkJoinPhased(2, new ArgCallable<Void, List<Interval>>() {
//						public Void run(List<Interval> phases) {
//							//debug("Phased, Subinterval phase="+phase+" interval="+interval);
//							phases.get(0).newChild(new AddTask(list, phase, 0));
//							phases.get(1).newChild(new AddTask(list, phase, 1));
//							phases.get(0).newChild(new AddTask(list, phase, 0));
//							phases.get(1).newChild(new AddTask(list, phase, 1));
//							return null;
//						}
//					});
//					return null;
//				}
//			}			
//			
//			Intervals.forkJoinPhased(2, new ArgCallable<Void, List<Interval>>() {
//				public Void run(List<Interval> phases) {
//					//debug("Phased, interval="+interval);
//					phases.get(0).newChild(new Subinterval(0));
//					phases.get(1).newChild(new Subinterval(1));
//					return null;
//				}
//			});			
//			
//			Assert.assertEquals("equal", 8, list.size());
//			checkOrdering(list);
//		}
//	}
//	
//	@Test public void illegalCreateChild() {
//		final boolean[] res = new boolean[1];		
//		Intervals.forkJoin(new Task() {
//			public Void run(Interval intervalImpl) throws Exception {				
//				Interval i = intervalImpl.newChild(new Empty());
//				try {
//					i.newChild(new Empty());
//				} catch (IntervalMustPreventFromClosingException e) {
//					res[0] = true;
//				}
//				return null;
//			}
//		});
//		Assert.assertTrue("createChild() did not throw exception", res[0]);
//	}
	
//	/**
//	 * Check for a cycle when a grandchild waits on its grandparent.
//	 */
//	@Test public void cyclicWaitGrandchild() {
//		final boolean[] res = new boolean[1];		
//		
//		Intervals.forkJoin(new Task() {
//			public Void run(final Interval parent) {
//				
//				Intervals.forkJoin(new Task() {
//					public Void run(final Interval child1) {
//						
//						Intervals.forkJoin(new Task() {
//							public Void run(final Interval child2) {
//								try {
//									child2.split(null, startAfter(end(parent)));
//								} catch (IntervalCannotWaitOnException e) {
//									res[0] = true;
//								}
//								return null;
//								
//							}
//						});
//						return null;
//						
//					}
//				});
//				return null;
//				
//			}
//		});
//		Assert.assertTrue("joinIntervalChain() did not throw exception", res[0]);
//	}
	
//	/** it's not ok to wait for your parents */
//	@Test public void illegalJoin() {
//		final boolean[] res = new boolean[1];		
//		Intervals.forkJoin(new Task() {
//			public Void run(final Interval parent) {				
//				parent.newChild(new Task() {
//					public Void run(final Interval child) {
//						try {
//							child.split(null, startAfter(end(parent)));
//						} catch (IntervalCannotWaitOnException e) {
//							res[0] = true;
//						}
//						return null;
//					}
//				});
//				return null;
//			}
//		});
//		Assert.assertTrue("join() did not throw exception", res[0]);
//	}
	
//	/** it's ok to wait for yourself */
//	@Test public void selfAwait() {
//		final boolean[] res = new boolean[1];
//		Intervals.forkJoin(new Task() {
//			public Void run(final Interval parent) {
//				try {
//					parent.split(null, startAfter(end(parent)));
//				} catch (IntervalCannotWaitOnException e) {
//					res[0] = true;
//				}
//				return null;
//			}
//		});
//		Assert.assertTrue("joinIntervalChain() threw exception", !res[0]);
//	}
	
//	/** it's ok to wait split as many times as you like, waiting on the same intervals, etc */ 
//	@Test public void splitTwice() {
//		@SuppressWarnings("unchecked")
//		final Interval[] i = new Interval[3];
//		Intervals.forkJoin(new Task() {
//			public Void run(Interval parent) {
//				Interval child = i[0] = parent.newChild(new Empty());
//				i[1] = parent = parent.split(null, startAfter(end(child)));
//				i[2] = parent.split(null, startAfter(end(child)));
//				return null;
//			}
//		});
//		Assert.assertNotSame(i[0], i[1]);
//		Assert.assertNotSame(i[1], i[2]);
//		Assert.assertNotSame(i[0], i[2]);
//	}
	
//	/**
//	 * That that any number children can wait for each other without a problem,
//	 * and that this acts as a barrier.
//	 */
//	@Test public void joinOpposite() {
//		for (int iter = 0; iter < repeat; iter++)
//			tryJoinOpposite();
//	}
//
//	private void tryJoinOpposite() {
//		final int parallelismLevel = Intervals.POOL.getParallelism();
//		final int length = parallelismLevel * 2;		
//		final int[] res = new int[length];
//		@SuppressWarnings("unchecked")
//		final Interval[] children = new Interval[length];
//		final AtomicInteger arrived = new AtomicInteger();
//
//		class BarrierTask extends AbstractTask {
//			
//			final int index;	
//			
//			public BarrierTask(int index) {
//				this.index = index;
//			}
//
//			public Void run(Interval arg) {
//				// in theory, no thread can proceed until they all
//				// have arrived, so add a delay...
//				debug("Child %d started [%s]", index, arg);
//				arrived.incrementAndGet();
//				debug("Child %d sleeping [%s]", index, arg);
//				arg = arg.split(null, children);	
//				res[index] = arrived.get();				
//				debug("Child %d done [%s]", index, arg);
//				return null;
//			}
//			
//		}
//		
//		Intervals.forkJoinSetup(new SetupTask() {
//			@Override
//			public Void setup(Interval current, Interval worker) {
//				for (int i = 0; i < length; i++) {
//					children[i] = worker.newChild(new BarrierTask(i));
//					debug("Spawning child %d==%s from %s", i, children[i], current);
//				}
//				debug("Next interval: %s", worker);
//				return null;
//			}
//		});
//
//		// both should read 2 from the values array.
//		for (int i = 0; i < length; i++)
//			Assert.assertEquals(length, res[i]);
//	}

	
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
							Interval nextInterval = intervalDuring(parent)
								.startAfter(iEnd)
								.startAfter(intervalFor(row, col - 1))
								.startAfter(intervalFor(row, col + 1))
								.schedule(nextCell);
							setIntervalFor(row + 1, col, nextInterval.end()); 
						}
					}
				}
				
				final AtomicInteger counter = new AtomicInteger(0);
				for (int c = 0; c < C; c++)
					setIntervalFor(0, c, intervalDuring(parent).schedule(new Cell(0, c, counter)).end());
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
		final Interval a = interval().schedule(Intervals.emptyTask);
		interval().endBefore(a.start()).schedule(Intervals.emptyTask);
	}
	
	@Test(expected=NoEdgeException.class) 
	public void raceConditionInBeforeGeneratesError2() {
		final Interval a = interval().schedule(Intervals.emptyTask);
		interval().startBefore(a.end()).schedule(Intervals.emptyTask);
	}
	
	@Test(expected=NoEdgeException.class) 
	public void raceConditionInBeforeGeneratesError3() {
		final Interval a = interval().schedule(Intervals.emptyTask);
		intervalWithBound(a.start()).schedule(Intervals.emptyTask);
	}	

	@Test(expected=CycleException.class) 
	public void simpleCycleGeneratesError() {
		interval().endAfter(Intervals.ROOT_END).schedule(emptyTask);
	}
	
	@Test 
	public void raceCondErrorsLeaveSchedulerInStableState() {
		final Interval a = interval().schedule(Intervals.emptyTask);
		final AtomicInteger i = new AtomicInteger();
		
		try {
			blockingInterval(new Task() {
				public void run(Point currentEnd) {
			
					interval()
					.endBefore(currentEnd)
					.schedule(new IncTask(i));
					
					interval()
					.endBefore(currentEnd)
					.endBefore(a.end())
					.schedule(Intervals.emptyTask);
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
			blockingInterval(new Task() {
				public void run(Point currentEnd) {
			
					interval()
					.endBefore(currentEnd)
					.schedule(new IncTask(i));
					
					interval()
					.endBefore(currentEnd)
					.endAfter(currentEnd)
					.schedule(Intervals.emptyTask);
				}			
			});
			
			Assert.fail("NoEdgeException never thrown");
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof CycleException);
		}
		
		Assert.assertEquals("Increment task failed to execute", 1, i.get());	
	}
}
