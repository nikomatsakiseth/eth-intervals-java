/**
 * 
 */
package erco.intervals.tsp;

import ch.ethz.intervals.InlineTask;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.Constructor;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;

public final class SplitTour extends InlineTask<TourElement> {
	private final int newNode;
	private final int wt;
	private final TourElement curr;
	private final Config config;

	public SplitTour(int newNode, int wt, TourElement curr, Config config) {
		this.newNode = newNode;
		this.wt = wt;
		this.curr = curr;
		this.config = config;
	}

	@Requires("method suspends Subinterval")
	@Override public TourElement run(Interval subinterval) {
		@Constructor("method") TourElement newTour = 
			new /*@Constructor("method")*/ TourElement(newNode);
		newTour.previous = curr;
		newTour.length += curr.length;
		newTour.visited |= curr.visited;
		newTour.prefixWeight = curr.prefixWeight + wt;
		newTour.lowerBound = calcBound(newTour);
		return newTour;
	}

	private int calcBound(@Constructor("readableBy method") TourElement newTour) {
		assert newTour.length < config.numNodes - 2;

		/*
		 * Add up the lowest weights for edges connected to vertices not yet
		 * used or at the ends of the current tour, and divide by two. This
		 * could be tweaked quite a bit. For example: (1) Check to make sure
		 * that using an edge would not make it impossible for a vertex to
		 * have degree two. (2) Check to make sure that the edge doesn't
		 * give some vertex degree 3.
		 */

		int mstWeight = 0;
		for (int i = 0; i < config.numNodes; i++) {
			if(newTour.visited(i)) continue;
			
			/*
			 * wt1: the value of the edge with the lowest weight from the node
			 * we're testing to another unconnected node. wt2: the value of the
			 * edge with the second lowest weight
			 */
			final int MAX_VALUE = 2147483647; // XXX Avoid static fields for now.
			int wt1 = MAX_VALUE, wt2 = MAX_VALUE;
			for (int j = 0; j < config.numNodes; j++) {
				/*
				 * Ignore j's that are not connected to i
				 * (global->weights[i][j]==0),
				 */
				/* or that are already in the tour and aren't either the */
				/* first or last node in the current tour. */
				int wt = config.weights[i][j];
				if(wt == 0) continue;				

				/* Might want to check that edges go to unused vertices */
				if (wt < wt1) {
					wt2 = wt1;
					wt1 = wt;
				} else if (wt < wt2)
					wt2 = wt;
			}

			/* At least three unconnected nodes? */
			if (wt2 != MAX_VALUE)
				mstWeight += ((wt1 + wt2) >> 1);
			
			/* Exactly two unconnected nodes? */
			else if (wt1 != MAX_VALUE)
				mstWeight += wt1;
		}
		mstWeight += 1;
		return mstWeight + newTour.prefixWeight;
	}		
}