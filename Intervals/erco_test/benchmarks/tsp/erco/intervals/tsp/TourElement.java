package erco.intervals.tsp;

/*
 * Copyright (C) 2000 by ETHZ/INF/CS
 * All rights reserved
 * 
 * @version $Id: TourElement.java 2094 2003-01-30 09:41:18Z praun $
 * @author Florian Schneider
 */
import ch.ethz.intervals.quals.Creator;

@Creator("Constructor")
public class TourElement 
implements Comparable<TourElement>
{
	TourElement previous;
	int node;
	int length;
	int visited;	
	int prefixWeight;
	int lowerBound;
	
	public TourElement(int node) {
		this.node = node;
		this.length = 1;		
		this.visited = (1 << node);
	}
	
	public boolean visited(int i) {
		return (visited & (1 << i)) != 0;
	}
	
	@Override
	public int compareTo(TourElement o) {
		return lowerBound - o.lowerBound; // XXX
	}
}
