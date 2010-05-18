package erco.intervals.tsp;

import java.util.PriorityQueue;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.LockImpl;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.ResultTask;

public class Config {
	private final LockImpl minLock;
	private final LockImpl queueLock;
	private final @Creator("queueLock") PriorityQueue<TourElement> queue;	
	final int numNodes;
	final int/*@Creator("Constructor")*/[]/*@Creator("Constructor")*/[] weights;
	
	@GuardedBy("Constructor") int startNode;
	@GuardedBy("Constructor") int nodesFromEnd;
	
	@GuardedBy("(RacyGuard#racy)") int minTourLength;
	@GuardedBy("minLock") int[] minTour;
	
	Config(int tspSize) {
		minLock = new LockImpl();
		queueLock = new LockImpl();
		queue = new PriorityQueue<TourElement>();
		numNodes = tspSize;
		weights = new int[numNodes + 1][numNodes + 1];
		minTour = new int[numNodes + 1];
		minTourLength = Integer.MAX_VALUE;
		nodesFromEnd = 12;
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
				subinterval.addLock(queueLock);
			}
			@Override public void run(Interval subinterval) {
				if(curDist < minTourLength) {
					System.arraycopy(path, 0, minTour, 0, minTour.length);
					minTourLength = curDist;
				}
			}
		});
	}

}
