/**
 * 
 */
package ch.ethz.intervals.visualizer;

public abstract class GraphWalker
{
	public abstract GraphWalker inverse();
	
	public abstract PointInfo start(IntervalInfo inter);
	public abstract PointInfo end(IntervalInfo inter);
	public abstract PointInfo opp(PointInfo pnt);	
	public abstract EdgeInfo edges(PointInfo pnt);
	public abstract EdgeInfo nextEdge(EdgeInfo prev);
	public abstract PointInfo point(EdgeInfo edge);
	
	public static class ForwardWalker extends GraphWalker 
	{
		public GraphWalker inverse() { return reverse; }
		public PointInfo start(IntervalInfo inter) { return inter.start; }
		public PointInfo end(IntervalInfo inter) { return inter.end; }
		public PointInfo opp(PointInfo pnt) { return (pnt.isStart() ? pnt.inter.end : null); }
		public EdgeInfo edges(PointInfo pnt) { return pnt.outgoing; }
		public EdgeInfo nextEdge(EdgeInfo prev) { return prev.nextOut; }
		public PointInfo point(EdgeInfo edge) { return edge.to; }
	}
	public static final ForwardWalker forward = new ForwardWalker();
	
	public static class ReverseWalker extends GraphWalker 
	{
		public GraphWalker inverse() { return forward; }
		public PointInfo start(IntervalInfo inter) { return inter.end; }
		public PointInfo end(IntervalInfo inter) { return inter.start; }
		public PointInfo opp(PointInfo pnt) { return (pnt.isStart() ? null : pnt.inter.start); }
		public EdgeInfo edges(PointInfo pnt) { return pnt.incoming; }
		public EdgeInfo nextEdge(EdgeInfo prev) { return prev.nextIn; }
		public PointInfo point(EdgeInfo edge) { return edge.from; }		
	}
	public static final ReverseWalker reverse = new ReverseWalker();	
}