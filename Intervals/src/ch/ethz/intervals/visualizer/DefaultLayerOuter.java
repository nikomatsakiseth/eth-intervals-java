package ch.ethz.intervals.visualizer;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class DefaultLayerOuter
extends BaseLayerOuter
implements LayerOuter
{
	private final boolean DEBUG_LAYOUT = false;
	
	public DefaultLayerOuter(
			ConflictMap conflictMap,
			GraphLayoutMetrics glm) 
	{
		super(conflictMap, glm);
	}
	
	private double score(int time, PointInfo pnt, int x, int row) {
		// Returns a goodness score for placing pnt at (x, y). 
		// The "score" of a location will be the
		// average of the squared lengths of all incoming edges.
		
		Iterator<PointInfo> preds = pnt.placedPreds().iterator();
		if(!preds.hasNext())
			return 0.0;
		
		final int y = y(row);
		int cnt = 0;
		double sum = 0.0;
		while(preds.hasNext()) {
			PointInfo inPnt = preds.next();
			Point coord = glm.outerCorner(inPnt);
			sum += Math.pow(x - coord.x, 2) + Math.pow(y - coord.y, 2);
			cnt += 1;
//			System.err.printf("[Pred: %s/%d]", inPnt, coord.x);
		}
		return sum / cnt;
	}
	
	private void clearDependentPoints(int time, Set<PointInfo> dep, PointInfo pnt) {
		if(dep.add(pnt)) {
			pnt.needsLayout = true;
			
			for(PointInfo succPnt : pnt.succs())
				clearDependentPoints(time, dep, succPnt);
			
			if(pnt.isStart()) {
				setBounds(time, pnt.inter, null);
				clearDependentPoints(time, dep, pnt.inter.end);
			}
		}
	}
	
	private void layoutPoint(int time, PointInfo pnt) {
		if(!pnt.needsLayout)
			return;
		pnt.needsLayout = false;
		
		for(PointInfo predPnt : pnt.preds())
			layoutPoint(time, predPnt);
		
		if(DEBUG_LAYOUT)
			System.err.printf("layout(%d, %s): (isStart=%s)\n", time, pnt, pnt.isStart());
		
		if(pnt.isStart()) {
			final int maxOfPredsX = maxOfPredsX(time, pnt);

			if(DEBUG_LAYOUT)
				System.err.printf("  maxOfPredsX = %d\n", maxOfPredsX);

			// Try to find the most suitable row of those that have been
			// allocated so far.  The "score" of a location will be the
			// average of the squared lengths of all incoming edges.
			int bestX = -1, bestRow = -1;
			double bestScore = Double.POSITIVE_INFINITY;
			int maxRow = maxRow();
			for(int row = 0; row <= maxRow; row++) {
				int x = maxOfPredsX;
				
				IntervalInfo rightmost = rightmost(row);
				if(rightmost != null) {
					if(rightmost.end.arrivalTime > time) {
						if(DEBUG_LAYOUT)
							System.err.printf("  row %d reserved by %s until %d\n", row, rightmost, rightmost.end.arrivalTime);
						continue; // reserved, end not yet arrived
					}
					x = Math.max(x, glm.outerCorner(rightmost.end).x);
				}
				
				//
				final double score = score(time, pnt, x, row);
				if(DEBUG_LAYOUT)
					System.err.printf("  row %d got score %f at x %d\n", row, score, x);
				if (score < bestScore) {
					bestX = x;
					bestRow = row;
					bestScore = score;
					if(score == 0.0) // will never find better 
						break;
				}
			}
				
			assert bestRow >= 0;
			
			Rectangle rect = new Rectangle();
			rect.x = bestX;
			rect.y = y(bestRow);
			rect.width = glm.intervalWidth(pnt.inter.label); // may grow, depends on end point deps
			rect.height = glm.intervalHeight;
			setBounds(time, pnt.inter, rect);
			if(DEBUG_LAYOUT)
				System.err.printf("  bounds=%s\n", rect);
		} else {
			layoutPoint(time, pnt.inter.start);
			Rectangle rect = new Rectangle(pnt.inter.bounds);
			int maxOfPredsX = maxOfPredsX(time, pnt);
			int label = glm.intervalWidth(pnt.inter.label);
			int finalX = Math.max(rect.x + label, maxOfPredsX);
			rect.width = finalX - rect.x;			
			setBounds(time, pnt.inter, rect);			
			if(DEBUG_LAYOUT)
				System.err.printf("  maxOfPredsX=%d, label=%d\n", maxOfPredsX, rect.x + label);
			if(DEBUG_LAYOUT)
				System.err.printf("  bounds=%s\n", rect);
		} 
	}
	
	private int maxOfPredsX(int time, PointInfo pnt) {
		int maxX = 0;
		for(PointInfo inPnt : pnt.placedPreds()) {
			Point coord = glm.outerCorner(inPnt);
			maxX = Math.max(maxX, coord.x + glm.minimumEdgeWidth + glm.pointGlyphWidth);
		}
		return maxX;
	}

	@Override
	public void layout(int time, List<PointInfo> inPnts) {
		if(DEBUG_LAYOUT)
			System.err.printf("\n--------\n\n");
		Set<PointInfo> allPnts = new HashSet<PointInfo>();
		for(PointInfo pnt : inPnts)
			clearDependentPoints(time, allPnts, pnt);

		for(PointInfo pnt : allPnts)
			layoutPoint(time, pnt);
	}

}
