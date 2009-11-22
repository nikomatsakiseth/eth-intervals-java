/**
 * 
 */
package ch.ethz.intervals;


public class EdgeList {
	
	final PointImpl toPoint;
	final EdgeList next;
	final boolean determinstic;
	
	public EdgeList(PointImpl toPoint, boolean deterministic, EdgeList next) {
		this.toPoint = toPoint;
		this.determinstic = deterministic;
		this.next = next;
	}
	
}