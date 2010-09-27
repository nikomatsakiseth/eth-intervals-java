package ch.ethz.intervals.tsp;

import java.util.PriorityQueue;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.ResultTask;

public class Config {
	private final Lock minLock;
	private final Lock queueLock;
	private final PriorityQueue<TourElement> queue;	
	final int numNodes;
	final int[][] weights;
	
	@GuardedBy("Constructor") int startNode;
	@GuardedBy("Constructor") int nodesFromEnd;
	
	private final CheckGuard<Integer> minTourLength;
	private final CheckGuard<int[]> minTour;
	
	Config(Interval search, int tspSize) {
		minLock = Intervals.lock("minLock");
		queueLock = Intervals.lock("queueLock");
		queue = new PriorityQueue<TourElement>();
		numNodes = tspSize;
		weights = new int[numNodes + 1][numNodes + 1];
		minTour = new CheckGuard<int[]>(
				new int[numNodes + 1], 
				minLock);
		minTourLength = new CheckGuard<Integer>(
				Integer.MAX_VALUE, 
				new MinTourLengthGuard(search, minLock));
		nodesFromEnd = 12;
	}
	
	int minTourLength() {
		return minTourLength.get();
	}
	
	TourElement getTour() {
		return Intervals.inline(new ResultTask<TourElement>() {
			@Override public void attachedTo(Interval subinterval) {
				subinterval.addLock(queueLock);
			}
			@Override public TourElement compute(Interval subinterval) {
				return queue.remove();
			}
		});
	}

	public void enqueue(final TourElement newTour) {
		Intervals.inline(new AbstractTask() {
			@Override public void attachedTo(Interval subinterval) {
				subinterval.addLock(queueLock);
			}
			@Override public void run(Interval subinterval) {
				queue.add(newTour);
			}
		});
	}
	
	public void setBest(final int curDist, final int[] path) {
		Intervals.inline(new AbstractTask() {
			@Override public void attachedTo(Interval subinterval) {
				subinterval.addLock(minLock);
			}
			@Override public void run(Interval subinterval) {
				if(curDist < minTourLength.get()) {
					int[] tour = minTour.get();
					System.arraycopy(path, 0, tour, 0, tour.length);
					minTourLength.set(curDist);
				}
			}
		});
	}

	public Result result() { // XXX Adjust guards so that lock is not req'd
		return Intervals.inline(new ResultTask<Result>() {
			@Override public void attachedTo(Interval subinterval) {
				subinterval.addLock(minLock);
			}

			@Override
			protected Result compute(Interval current) throws Exception {
				return new Result(minTourLength.get(), minTour.get());
			}
		});
	}

}
