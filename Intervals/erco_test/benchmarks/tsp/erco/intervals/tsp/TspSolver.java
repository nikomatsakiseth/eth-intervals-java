package erco.intervals.tsp;

import java.util.BitSet;

import ch.ethz.intervals.Dependency;
import ch.ethz.intervals.InlineTask;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.ParentForNew;
import ch.ethz.intervals.quals.Constructor;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Requires;

public class TspSolver extends Interval {
	
	final @Creator("readableBy this") Config config;
	
	@GuardedBy("this") int curDist;
	@GuardedBy("this") int pathLen;
	@GuardedBy("this") int path[];
	@GuardedBy("this") BitSet visited;

	public TspSolver(
			@ParentForNew("Parent") Dependency dep,
			@Creator("readableBy this") Config config
	) {
		super(dep);
		this.config = config;
	}

	@Override
	@Requires("method suspends this")
	protected void run() {
		TourElement curr = config.getTour();
		
		if (curr.length < (config.numNodes - config.nodesFromEnd - 1))
			splitTour(curr); /* Solve in parallel. */
		else
			solveTour(curr); /* Solve sequentially */
		
	}

	private void splitTour(final TourElement curr) {
		/*
		 * Create a tour and add it to the priority Q for each possible path
		 * that can be derived from the current path by adding a single node
		 * while staying under the current minimum tour length.
		 */

		for (int i = 0; i < config.numNodes; i++) {
			/*
			 * Check: 1. Not already in tour 2. Edge from last entry to
			 * node in graph and 3. Weight of new partial tour is less
			 * than cur min.
			 */
			final int wt = config.weights[curr.node][i];
			boolean t1 = !curr.visited(i); 
			boolean t2 = (wt != 0);
			boolean t3 = (curr.lowerBound + wt) <= config.minTourLength;
			if (t1 && t2 && t3) {					
				final int newNode = i;
				TourElement newTour = Intervals.inline(new InlineTask<TourElement>() {
					@Override public TourElement run(Interval subinterval) {
						@Constructor("initNewTour") TourElement newTour = 
							new /*@Constructor("initNewTour")*/ TourElement(newNode);
						newTour.previous = curr;
						newTour.length += curr.length;
						newTour.visited |= curr.visited;
						newTour.prefixWeight = curr.prefixWeight + wt;
						newTour.lowerBound = calcBound(newTour);
						return newTour;
					}
				});
				
				if(newTour != null) {
					config.enqueue(newTour);
					new TspSolver(parent, config);
				}					
			}
		}
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
			int wt1 = Integer.MAX_VALUE, wt2 = Integer.MAX_VALUE;
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
			if (wt2 != Integer.MAX_VALUE)
				mstWeight += ((wt1 + wt2) >> 1);
			
			/* Exactly two unconnected nodes? */
			else if (wt1 != Integer.MAX_VALUE)
				mstWeight += wt1;
		}
		mstWeight += 1;
		return mstWeight + newTour.prefixWeight;
	}

	private void solveTour(TourElement curr) {
		curDist = curr.prefixWeight;
		pathLen = curr.length;
		visited = new BitSet(config.numNodes);
		path = new int[config.numNodes+1];
		
//		StringBuilder sb = new StringBuilder("SolveTour:");
		
		TourElement p = curr;
		for (int i = pathLen - 1; i >= 0; i--) {
//			sb.append(String.format(" %d", p.node));
			path[i] = p.node;
			visited.set(p.node);
			p = p.previous;			
		}
		
//		System.out.println(sb);

		visitNodes(path[pathLen - 1]);
	}


	/*
	 * visit_nodes()
	 * 
	 * Exhaustively visits each node to find Hamilton cycle. Assumes that search
	 * started at node from.
	 */
	@Requires("method suspends this")
	void visitNodes(int from) {
		int i;
		int dist, last;

		for (i = 1; i < config.numNodes; i++) {
			if (visited.get(i))
				continue; /* Already visited. */
			if ((dist = config.weights[from][i]) == 0)
				continue; /* Not connected. */
			if (curDist + dist > config.minTourLength)
				continue; /* Path too long. */

			/* Try next node. */
			visited.set(i);
			path[pathLen++] = i;
			curDist += dist;

			if (pathLen == config.numNodes) {
				/* Visiting last node - determine if path is min length. */
				if ((last = config.weights[i][config.startNode]) != 0
						&& (curDist += last) < config.minTourLength) 
				{
					path[pathLen] = config.startNode;
					config.setBest(curDist, path);
				}
				curDist -= last;
			} /* if visiting last node */
			else if (curDist < config.minTourLength)
				visitNodes(i); /* Visit on. */

			/* Remove current try and loop again at this level. */
			curDist -= dist;
			pathLen--;
			visited.clear(i);
		}
	}
	
}
