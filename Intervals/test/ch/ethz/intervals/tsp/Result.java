package ch.ethz.intervals.tsp;

public class Result {
	final public int minTourLength;
	final public int minTour[];
	
	public Result(int minTourLength, int[] minTour) {
		this.minTourLength = minTourLength;
		this.minTour = minTour;
	}
}