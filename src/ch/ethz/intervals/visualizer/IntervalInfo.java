package ch.ethz.intervals.visualizer;

import java.awt.Rectangle;

public class IntervalInfo {
	
	public final PointInfo start;  /** */
	public final PointInfo end;    /** */
	public final int id;           /** unique among intervals, small integer */
	public Rectangle bounds;       /** set by layout engine */
	public String label;           /** set when scheduled */
	
	public IntervalInfo(
			int intervalId,
			int startId, 
			int endId)
	{
		this.label = "..."; // will be changed later
		this.id = intervalId;
		this.start = new PointInfo(this, startId);
		this.end = new PointInfo(this, endId);
	}
	
	@Override
	public String toString() {
		return String.format("Inter(%d,%d-%d)", 
				id, start.id, end.id);
	}
	
	public void adjustSuperfluousEdgesToPreds(int amnt) {
		
		for(PointInfo succ : start.succs())
			for(PointInfo pred : start.allPreds())
				pred.adjustSuperfluousSucc(amnt, succ);
		
		for(PointInfo succ : end.succs())
			for(PointInfo pred : end.allPreds())
				pred.adjustSuperfluousSucc(amnt, succ);
		
	}

	public void adjustSuperfluousEdgesFromPreds(int amnt) {		
		for(EdgeInfo pred0 : start.predEdges()) {
			for(EdgeInfo pred1 : start.predEdges()) {
				if(pred0 != pred1) {
					if(pred0.from.hb(pred1.from)) {
						pred0.superfluous += amnt;
					}
				}
			}
		}	
		
		for(EdgeInfo pred0 : end.predEdges()) {
			if(pred0.from.hb(start)) {
				pred0.superfluous += 1;
			} else for(EdgeInfo pred1 : end.predEdges()) {
				if(pred0 != pred1) {
					if(pred0.from.hb(pred1.from)) {
						pred0.superfluous += amnt;
					}
				}
			}
		}	
	}
	
}
