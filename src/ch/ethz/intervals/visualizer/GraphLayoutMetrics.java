/**
 * 
 */
package ch.ethz.intervals.visualizer;

import java.awt.FontMetrics;
import java.awt.Point;
import java.awt.Rectangle;

class GraphLayoutMetrics {
	
	/** 
	 * Width and height of the glyph for a point in pixels.
	 */
	final int pointGlyphWidth, pointGlyphHeight;
	
	/** 
	 * Minimum number of pixels to leave between the start/end points 
	 * and the label.
	 */
	final int labelGapWidth;
	
	/** 
	 * Gap between rows (in pixels).
	 */
	final int rowGapHeight;
	
	/** 
	 * Overall height of an interval in pixels.
	 */
	final int intervalHeight;
	
	/**
	 * Width of an edge.
	 */
	final int edgeStrokeWidth;
	
	/**
	 * Width of the ring around the selected interval.
	 */
	final int selectionStrokeWidth;
	
	/**
	 * Metrics for current font to determine width of a label etc.
	 */
	final FontMetrics fontMetrics;
	
	/**
	 * Minimum horizontal distance to leave between two points
	 * which are connected by an edge. 
	 */
	final int minimumEdgeWidth; 
	
	public GraphLayoutMetrics(
			int pointWidth,
			int pointHeight, 
			int labelGap, 
			int verticalGapHeight,
			int minimumEdgeWidth,
			int edgeStrokeWidth,
			int selectionStrokeWidth,
			FontMetrics fontMetrics) 
	{
		this.pointGlyphWidth = pointWidth;
		this.pointGlyphHeight = pointHeight;
		this.labelGapWidth = labelGap;
		this.fontMetrics = fontMetrics;
		this.minimumEdgeWidth = minimumEdgeWidth;
		this.edgeStrokeWidth = edgeStrokeWidth;
		this.rowGapHeight = verticalGapHeight;
		this.selectionStrokeWidth = selectionStrokeWidth;
		
		this.intervalHeight = Math.max(pointHeight, fontMetrics.getHeight());
	}
	
	/**
	 * Returns the (minimum) width of an interval with the given label. 
	 */
	public int intervalWidth(String label) {
		return pointGlyphWidth * 2 + labelGapWidth * 2 + fontMetrics.stringWidth(label);
	}
	
	/**
	 * Intervals are layed out in horizontal rows.  This is the height of such
	 * a row.  Within a row, the actual height of the interval is {@link #intervalHeight},
	 * which is slightly less than the height of the row, because there is a small gap
	 * between row.
	 */
	public int rowHeight() {
		return intervalHeight + this.rowGapHeight;
	}
	
	/**
	 * Returns a point along the top border, either at the
	 * left or right depending on whether inPnt is a start or end.
	 * Assumes that the rectangle for inPnt is initialized.
	 */
	public Point outerCorner(PointInfo pnt) {
		Rectangle rect = pnt.inter.bounds;
		if(pnt.isStart())
			return new Point(rect.x, rect.y);
		else
			return new Point(rect.x + rect.width, rect.y);
	}
	
	public Point ulCorner(PointInfo pnt) {
		Rectangle rect = pnt.inter.bounds;
		if(pnt.isStart())
			return new Point(rect.x, rect.y);
		else
			return new Point(rect.x + rect.width - pointGlyphWidth, rect.y);
	}

	public Point center(PointInfo pnt) {
		Point corner = ulCorner(pnt);
		corner.x += pointGlyphWidth / 2;
		corner.y += pointGlyphHeight / 2;
		return corner;
	}
	
}