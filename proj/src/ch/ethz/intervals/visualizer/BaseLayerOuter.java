package ch.ethz.intervals.visualizer;

import java.awt.Rectangle;

public class BaseLayerOuter {

	protected final ConflictMap conflictMap;
	protected final GraphLayoutMetrics glm;
	
	public BaseLayerOuter(ConflictMap conflictMap, GraphLayoutMetrics glm) {
		this.conflictMap = conflictMap;
		this.glm = glm;
	}

	protected int row(int y) {
		return y / glm.rowHeight();
	}

	protected int y(int row) {
		return row * glm.rowHeight();
	}

	protected IntervalInfo rightmost(int row) {
		return conflictMap.rightmost(row);
	}

	/** Returns the maximum row that may have an occupant. */
	protected int maxRow() {
		return row(conflictMap.computeSize().height) + 1; 
	}
		
	protected void setBounds(int time, IntervalInfo inter, Rectangle bounds) {
		if(inter.bounds != null)
			conflictMap.remove(inter);
		
		inter.bounds = bounds;
		
		if(inter.bounds != null)
			conflictMap.add(inter);
	}

}
