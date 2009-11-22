package ch.ethz.intervals.visualizer;

public class EdgeInfo {
	
	public final PointInfo from;
	public final PointInfo to;
	
	public EdgeInfo nextOut;
	public EdgeInfo nextIn;
	
	public int superfluous;

	public EdgeInfo(PointInfo from, PointInfo to, EdgeInfo nextFrom,
			EdgeInfo nextTo) {
		this.from = from;
		this.to = to;
		this.nextOut = nextFrom;
		this.nextIn = nextTo;
	}

}
