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
	
	class IncTask implements Task<Integer> {
		public final AtomicInteger i;

		public IncTask(AtomicInteger i) {
			this.i = i;
		}

		@Override
		public Integer run(Interval<Integer> current) {
			return i.getAndIncrement();
		}		
	}
	
	class AddTask implements Task<Void> {

		public final List<List<Integer>> list;
		public final List<Integer> id;
		
		public AddTask(List<List<Integer>> list, Integer... ids) {
			this.id = Arrays.asList(ids);
			this.list = list;
		}

		public Void run(Interval<Void> i) {
			debug("%s", toString());
			list.add(id);
			return null;
		}
		
		@Override
		public String toString() {
			return "AddTask id="+id;
		}
		
	}
	
	class Empty implements Task<Void> {
		public Void run(Interval<Void> i) {
			return null;
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
			Intervals.blockingInterval(new Task<Void>() {
				public Void run(final Interval<Void> parent) {
					
					Intervals.blockingInterval(new Task<Void>() {
						public Void run(final Interval<Void> child) {
							
							intervalDuring(parent).startAfter(end(child)).schedule(new AddTask(list, 2));
							intervalDuring(child).schedule(new AddTask(list, 1));
							intervalDuring(child).schedule(new AddTask(list, 1));
							intervalDuring(child).schedule(new AddTask(list, 1));
							return null;
							
						}
					});
					return null;
					
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
		Integer r = Intervals.blockingSetupInterval(new SetupTask<Integer>() {
			@Override
			public Integer setup(Interval<Integer> current, Interval<Void> worker) {
				IntervalImpl<?> d = (IntervalImpl<?>) intervalDuring(worker).schedule(emptyTask);
				IntervalImpl<?> k = (IntervalImpl<?>) intervalDuring(d).schedule(emptyTask);
				IntervalImpl<?> n = (IntervalImpl<?>) intervalDuring(d).schedule(emptyTask);
				IntervalImpl<?> a = (IntervalImpl<?>) intervalDuring(worker).schedule(emptyTask);
				IntervalImpl<?> m = (IntervalImpl<?>) intervalDuring(a).schedule(emptyTask);
				IntervalImpl<?> t = (IntervalImpl<?>) intervalDuring(a).schedule(emptyTask);
				IntervalImpl<?> s = (IntervalImpl<?>) intervalDuring(t).schedule(emptyTask);
				
				Assert.assertEquals(d.end, d.end.mutualBound(d.end));
				Assert.assertEquals(d.end, k.end.mutualBound(n.end));
				Assert.assertEquals(worker.end(), k.end.mutualBound(s.end));
				Assert.assertEquals(worker.end(), s.end.mutualBound(k.end));
				Assert.assertEquals(a.end, m.end.mutualBound(s.end));
				Assert.assertEquals(a.end, s.end.mutualBound(m.end));
				return 22;
			}
		});
		Assert.assertEquals(Integer.valueOf(22), r);
	}
	
	class TestException extends RuntimeException { 
		
	}
	
	/**
	 * Tests that uncaught exceptions propagate up to the parent, etc.
	 */
	@Test public void exceptionPropagatesToParent() {
		class TestHarness {
			public void test(final boolean maskExceptions) {
				try {
					Intervals.blockingInterval(new Task<Void>() {

						@Override
						public Void run(Interval<Void> current) {
							
							intervalDuring(current)
							.setMaskExceptions(maskExceptions)
							.schedule(new Task<Void>() {

								@Override
								public Void run(Interval<Void> current) {
									throw new TestException();
								}
								
							});
							
							return null;
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
					Intervals.blockingInterval(new Task<Void>() {

						@Override
						public Void run(Interval<Void> current) {
							
							intervalDuring(current)
							.setMaskExceptions(maskExceptions)
							.schedule(new SubintervalWrapper(new Task<Void>() {

								@Override
								public Void run(Interval<Void> current) {
									throw new TestException();
								}
								
							}));
							
							return null;
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
//			class Subinterval implements Task<Void> {
//				int phase;
//
//				public Subinterval(int phase) {
//					this.phase = phase;
//				}								
//				
//				public Void run(Interval<Void> _) {
//					Intervals.forkJoinPhased(2, new ArgCallable<Void, List<Interval<Void>>>() {
//						public Void run(List<Interval<Void>> phases) {
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
//			Intervals.forkJoinPhased(2, new ArgCallable<Void, List<Interval<Void>>>() {
//				public Void run(List<Interval<Void>> phases) {
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
//		Intervals.forkJoin(new Task<Void>() {
//			public Void run(Interval<Void> intervalImpl) throws Exception {				
//				Interval<Void> i = intervalImpl.newChild(new Empty());
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
//		Intervals.forkJoin(new Task<Void>() {
//			public Void run(final Interval<Void> parent) {
//				
//				Intervals.forkJoin(new Task<Void>() {
//					public Void run(final Interval<Void> child1) {
//						
//						Intervals.forkJoin(new Task<Void>() {
//							public Void run(final Interval<Void> child2) {
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
//		Intervals.forkJoin(new Task<Void>() {
//			public Void run(final Interval<Void> parent) {				
//				parent.newChild(new Task<Void>() {
//					public Void run(final Interval<Void> child) {
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
//		Intervals.forkJoin(new Task<Void>() {
//			public Void run(final Interval<Void> parent) {
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
//		final Interval<Void>[] i = new Interval[3];
//		Intervals.forkJoin(new Task<Void>() {
//			public Void run(Interval<Void> parent) {
//				Interval<Void> child = i[0] = parent.newChild(new Empty());
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
//		final Interval<Void>[] children = new Interval[length];
//		final AtomicInteger arrived = new AtomicInteger();
//
//		class BarrierTask implements Task<Void> {
//			
//			final int index;	
//			
//			public BarrierTask(int index) {
//				this.index = index;
//			}
//
//			public Void run(Interval<Void> arg) {
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
//		Intervals.forkJoinSetup(new SetupTask<Void>() {
//			@Override
//			public Void setup(Interval<Void> current, Interval<Void> worker) {
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
		@SuppressWarnings("unchecked")
		final Interval<Void>[][] intervals = new Interval[2][C+2];
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
		Intervals.blockingSetupInterval(new SetupTask<Void>() {
			
			public Interval<Void> intervalFor(int row, int col) {
				return intervals[row & 1][col + 1];
			}

			public void setIntervalFor(int row, int col, Interval<Void> i) {
				intervals[row & 1][col + 1] = i;
			}
			
			public Void setup(Interval<Void> phase0, final Interval<Void> parent) {				
				class Cell implements Task<Void> {
					// coordinate of data:
					final int row, col;
					
					// shared counter indicating how many cells to process:
					final AtomicInteger counter;
					
					public Cell(int row, int col, AtomicInteger counter) {
						this.row = row;
						this.col = col;
						this.counter = counter;
					}

					public Void run(Interval<Void> i) {
						assert intervalFor(row, col) == i : String.format(
								"Invalid interval at (%d,%d): expected %s but found %s", 
								row, col, i, intervalFor(row, col));
						
						int value = counter.getAndIncrement();
						//debug("Cell @ %d,%d: value=%d interval=%s", row, col, value, i);
						
						if (value > dataPoints)
							return null;
						
						data[row][col] = value;
						
						// if not maximum row, proceed to next row
						if (row < R) {
							Cell nextCell = new Cell(row+1, col, counter);
							Interval<Void> nextInterval = intervalDuring(parent)
								.startAfter(end(i))
								.startAfter(end(intervalFor(row, col - 1)))
								.startAfter(end(intervalFor(row, col + 1)))
								.schedule(nextCell);
							setIntervalFor(row + 1, col, nextInterval); 
						}
						
						return null;
					}
				}
				
				final AtomicInteger counter = new AtomicInteger(0);
				for (int c = 0; c < C; c++)
					setIntervalFor(0, c, intervalDuring(parent).schedule(new Cell(0, c, counter)));
				return null;
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
	
	@Test public void passDataAlongChain() {
		for (int iter = 0; iter < repeat; iter++)
			tryPassDataAlongChain();
	}
	
	public void tryPassDataAlongChain() {
		// Simple dataflow that just passes a value along:
		final String expectedResult = "secret string";
		
		Task<String> seed = new Task<String>() {
			public String run(Interval<String> arg)
			{
				return expectedResult;
			}
		};
		
		final String actualResult = createChain(seed);
		Assert.assertEquals(expectedResult, actualResult);
	}

	@Test public void passErrorAlongChain() {
		for (int iter = 0; iter < repeat; iter++)
			tryPassErrorAlongChain();
	}
	
	public void tryPassErrorAlongChain() {
		// Simple dataflow that just passes a value along:
		final String expectedResult = "secret string";
		
		Task<String> seed = new Task<String>() {
			public String run(Interval<String> arg)
			{
				throw new RuntimeException(expectedResult);
			}
		};

		try {
			final String result = createChain(seed);
			Assert.fail("No exception thrown, but one was expected.  Resulted in: " + result);
		} catch (RethrownException e) {
			int links = 0;
			Throwable cause = e;
			while (cause.getCause() != null) {
				cause = cause.getCause();
				links++;
			}
			String msg = cause.getMessage();
			Assert.assertEquals(expectedResult, msg);
			Assert.assertEquals(chainLinks + 2, links); // forkJoin wraps, as does waitForChild()
		}
	}	
	
	final int chainLinks = 25;
	private String createChain(final Task<String> firstLink) {
		
		return Intervals.blockingInterval(new Task<String>() {

			public String run(Interval<String> parent) {
				
				// First child simply produces the value:
				Interval<String> child = intervalDuring(parent).schedule(firstLink);
				
				// Links pass it along:
				for (int i = 0; i < chainLinks; i++) {
					final EndPoint<String> previousChild = child.end();
					Task<String> link = new Task<String>() {
						public String run(Interval<String> arg)
						{
							return previousChild.result();
						}
					};
					child = intervalDuring(parent).startAfter(previousChild).schedule(link);
				}

				// Wait for the entire queue to terminate and return result:
				return Intervals.blockOn(child.end());
			}

		});
	}
	
	@Test(expected=NoEdgeException.class) 
	public void raceConditionInBeforeGeneratesError1() {
		final Interval<Void> a = interval().schedule(Intervals.emptyTask);
		interval().endBefore(a.start()).schedule(Intervals.emptyTask);
	}
	
	@Test(expected=NoEdgeException.class) 
	public void raceConditionInBeforeGeneratesError2() {
		final Interval<Void> a = interval().schedule(Intervals.emptyTask);
		interval().startBefore(a.end()).schedule(Intervals.emptyTask);
	}
	
	@Test(expected=NoEdgeException.class) 
	public void raceConditionInBeforeGeneratesError3() {
		final Interval<Void> a = interval().schedule(Intervals.emptyTask);
		intervalWithBound(a.start()).schedule(Intervals.emptyTask);
	}	

	@Test(expected=CycleException.class) 
	public void simpleCycleGeneratesError() {
		interval().endAfter(Intervals.ROOT_END).schedule(emptyTask);
	}
	
	@Test 
	public void raceCondErrorsLeaveSchedulerInStableState() {
		final Interval<Void> a = interval().schedule(Intervals.emptyTask);
		final AtomicInteger i = new AtomicInteger();
		
		try {
			blockingInterval(new Task<Void>() {
				public Void run(Interval<Void> current) {
			
					interval()
					.endBefore(current.end())
					.schedule(new IncTask(i));
					
					interval()
					.endBefore(current.end())
					.endBefore(a.end())
					.schedule(Intervals.emptyTask);
					
					return null;
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
			blockingInterval(new Task<Void>() {
				public Void run(Interval<Void> current) {
			
					interval()
					.endBefore(current.end())
					.schedule(new IncTask(i));
					
					interval()
					.endBefore(current.end())
					.endAfter(current.end())
					.schedule(Intervals.emptyTask);
					
					return null;
				}			
			});
			
			Assert.fail("NoEdgeException never thrown");
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof CycleException);
		}
		
		Assert.assertEquals("Increment task failed to execute", 1, i.get());	
	}
}
