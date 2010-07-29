package erco.intervals.tsp;

import java.util.PriorityQueue;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.ResultTask;

public class Config {
	private final Lock minLock;
	private final Lock queueLock;
	private final @Creator("queueLock") PriorityQueue<TourElement> queue;	
	final int numNodes;
	final int/*@Creator("Constructor")*/[]/*@Creator("Constructor")*/[] weights;
	
	@GuardedBy("Constructor") int startNode;
	@GuardedBy("Constructor") int nodesFromEnd;
	
	@GuardedBy("(RacyGuard#racy)") int minTourLength;
	@GuardedBy("minLock") int[] minTour;
	
	Config(int tspSize) {
		minLock = Intervals.lock("minLock");
		queueLock = Intervals.lock("queueLock");
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
				subinterval.addLock(minLock);
			}
			@Override public void run(Interval subinterval) {
//				System.err.printf("curDist: %d minTourLength: %d tour: %s\n", 
//						curDist, minTourLength, Arrays.toString(path));
				if(curDist < minTourLength) {
					System.arraycopy(path, 0, minTour, 0, minTour.length);
					minTourLength = curDist;
					
//					System.err.printf("  BEST SO FAR\n");
				}
			}
		});
	}

}
