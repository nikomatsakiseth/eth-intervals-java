package ch.ethz.intervals.tsp;

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
