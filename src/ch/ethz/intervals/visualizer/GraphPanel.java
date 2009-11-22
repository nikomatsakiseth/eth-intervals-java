package ch.ethz.intervals.visualizer;

import static java.util.Arrays.asList;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.swing.JPanel;

import ch.ethz.intervals.visualizer.EventLog.Arrive;
import ch.ethz.intervals.visualizer.EventLog.Edge;
import ch.ethz.intervals.visualizer.EventLog.Lock;
import ch.ethz.intervals.visualizer.EventLog.NewInterval;
import ch.ethz.intervals.visualizer.EventLog.Schedule;

@SuppressWarnings("serial")
public class GraphPanel extends JPanel {
	static final boolean DEBUG_CTION = false;
	static final boolean DEBUG_PAINT = false;

	public static void selectFont(Graphics g) {
		Font f = new Font(Font.MONOSPACED, Font.PLAIN, 12);
		g.setFont(f);
	}
	
	public static Graphics2D setAntiAliased(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
//        g2.setRenderingHint(
//        		RenderingHints.KEY_TEXT_ANTIALIASING,
//        		RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        g2.setRenderingHint(
        		RenderingHints.KEY_RENDERING,
        		RenderingHints.VALUE_RENDER_QUALITY);	
        g2.setRenderingHint(
        		RenderingHints.KEY_ANTIALIASING, 
        		RenderingHints.VALUE_ANTIALIAS_ON);
        return g2;
	}
	
	public final Color bgColors[] = new Color[] {
			Color.blue, Color.red, Color.darkGray
		};
	public final Color pointColor = Color.black; 
	public final Color labelColor = Color.white; 
	public final Color edgeColor = Color.black; 
	public final GraphLayoutMetrics glm;
	
	public final EventLogVisitor doVisitor, undoVisitor;
		
	private final ConflictMap conflictMap;
	private final IdMap<PointInfo> pointsById = new IdMap<PointInfo>();
	private int numIntervals = 0;
	private final LayerOuter layerOuter;	
	private IntervalInfo selectedInterval = null;
	
	public GraphPanel(ConflictMap conflictMap, GraphLayoutMetrics glm, LayerOuter layerOuter) {
		this.conflictMap = conflictMap;
		this.glm = glm;		
		this.layerOuter = layerOuter;
		doVisitor = new DoVisitor();
		undoVisitor = new UndoVisitor();
		this.setLayout(null); // I do my own layout thank you very much.
		this.setOpaque(false);
	}

	@Override
	public Dimension getPreferredSize() {
		return conflictMap.computeSize();
	}	
	
	public IntervalInfo intervalAt(Point p) {
		return conflictMap.get(p);
	}
	
	public void setSelectedInterval(IntervalInfo i) {
		this.selectedInterval = i;
		repaint();
	}
	
	public PointInfo pointById(int id) {
		return pointsById.get(id);
	}
	
	class DoVisitor implements EventLogVisitor {

		@Override
		public void visitNewInterval(NewInterval event, int time) {
			PointInfo current = pointsById.get(event.currentId);
			PointInfo endBound = pointsById.get(event.endBoundId);
			
			IntervalInfo newInter = new IntervalInfo(numIntervals++, event.startId, event.endId);
			pointsById.set(event.startId, newInter.start);
			pointsById.set(event.endId, newInter.end);
			
			if(current != null)
				current.connectTo(newInter.start);
			if(endBound != null)
				newInter.end.connectTo(endBound);
			
			if(DEBUG_CTION)
				System.err.printf("Event %d: NEW(%s) current=%s endBound=%s\n",
						time, newInter, current, endBound);
		}

		@Override
		public void visitEdge(Edge event, int time) {
			PointInfo from = pointsById.get(event.fromPointId);
			PointInfo to = pointsById.get(event.toPointId);
			
			if(from == null || to == null)
				return;
			
			from.connectTo(to);	
			
			if(DEBUG_CTION)
				System.err.printf("Event %d: Edge(%s - %s)\n", time, from, to);
		}

		@Override
		public void visitLock(Lock event, int index) {
		}

		@Override
		public void visitSchedule(Schedule event, int index) {
			PointInfo pnt = pointsById.get(event.startPointId);
			if(pnt == null)
				return;
			
			if(DEBUG_CTION)
				System.err.printf("Event %d: Schedule(%s, %s)\n", index, pnt, event.taskDescr);
			
			pnt.inter.adjustSuperfluousEdgesToPreds(1);
			pnt.inter.adjustSuperfluousEdgesFromPreds(1);
			
			pnt.inter.label = event.taskDescr;
			layerOuter.layout(index, asList(pnt));
		}
		
		@Override
		public void visitArrive(Arrive event, int index) {
			PointInfo pnt = pointsById.get(event.pointId);
			if(pnt == null)
				return;
			
			if(DEBUG_CTION)
				System.err.printf("Event %d: Arrive(%s)\n", index, pnt);
			
			pnt.arrivalTime = index;
			layerOuter.layout(index, asList(pnt));
		}

	}

	class UndoVisitor implements EventLogVisitor {

		@Override
		public void visitNewInterval(NewInterval event, int index) {
			PointInfo createdStart = pointsById.get(event.startId);
			PointInfo createdEnd  = pointsById.get(event.endId);
			
			if(createdStart.inter.bounds != null)
				conflictMap.remove(createdStart.inter);
			
			List<PointInfo> affectedPoints = new ArrayList<PointInfo>();
			for(PointInfo succ : createdStart.succs())
				affectedPoints.add(succ);
			for(PointInfo succ : createdEnd.succs())
				affectedPoints.add(succ);
			
			createdStart.inter.adjustSuperfluousEdgesToPreds(-1);
			
			// No need to adjust incoming superfluous edges, as we will remove them
			// anyhow.
			//createdStart.inter.adjustSuperfluousEdgesFromPreds(-1);
			
			numIntervals--;	
			createdStart.remove();
			createdEnd.remove();
			if(selectedInterval == createdStart.inter)
				selectedInterval = null;
			
			pointsById.set(event.startId, null);
			pointsById.set(event.endId, null);
			
			layerOuter.layout(index - 1, affectedPoints);
		}

		@Override
		public void visitEdge(Edge event, int index) {
			// we wait to remove edges until we get to the new interval creation
		}

		@Override
		public void visitLock(Lock event, int index) {
		}

		@Override
		public void visitSchedule(Schedule event, int index) {
			// When walking backwards, we wait to adjust the layout until we get 
			// to the new interval creation.
		}
		
		@Override
		public void visitArrive(Arrive event, int index) {
			PointInfo pnt = pointsById.get(event.pointId);
			if(pnt != null)
				layerOuter.layout(index - 1, asList(pnt));
		}
		
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);
		
//		g = g.create();
		selectFont(g);		
		Graphics2D g2 = setAntiAliased(g);
		
		Rectangle clipBounds = g2.getClipBounds();		
		
		if(DEBUG_PAINT)
			System.err.printf("paintComponent: %s bounds: %s\n", clipBounds, getBounds());
		
		Set<IntervalInfo> adjacentIntervals = new HashSet<IntervalInfo>();
		if(selectedInterval != null) {
			for(PointInfo pnt : selectedInterval.start.preds())
				adjacentIntervals.add(pnt.inter);
			for(PointInfo pnt : selectedInterval.start.succs())
				adjacentIntervals.add(pnt.inter);
			for(PointInfo pnt : selectedInterval.end.preds())
				adjacentIntervals.add(pnt.inter);
			for(PointInfo pnt : selectedInterval.end.succs())
				adjacentIntervals.add(pnt.inter);
		}

		List<IntervalInfo> intersecting = conflictMap.getAllIntersecting(clipBounds);
		
//		// Clear everything
//		g2.clearRect(clipBounds.x, clipBounds.y, clipBounds.width, clipBounds.height);
		
		// Draw edges
		g2.setColor(edgeColor);
		g2.setStroke(new BasicStroke(glm.edgeStrokeWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
		for(IntervalInfo inter : intersecting) {
			drawEdges(g2, inter.start, inter.start.nonSuperfluousPlacedPreds());
			drawEdges(g2, inter.start, inter.start.nonSuperfluousPlacedSuccs());
			drawEdges(g2, inter.end, inter.end.nonSuperfluousPlacedPreds());
			drawEdges(g2, inter.end, inter.end.nonSuperfluousPlacedSuccs());
		}
		
		// Draw intervals
		for(IntervalInfo inter : intersecting) {
			drawInterval(g2, adjacentIntervals, inter);
		}
	}
	
	private final float OPACITY_MAX = 1.0f; 
	private final float OPACITY_HALF = 0.5f; 
	private final float OPACITY_MIN = 0.2f; 
	
	private float opacity(Set<IntervalInfo> adjacentIntervals, IntervalInfo inter) {
		if(selectedInterval == null || inter == selectedInterval)
			return OPACITY_MAX;
		if(adjacentIntervals.contains(inter))
			return OPACITY_HALF;
		return OPACITY_MIN;
	}
	
	private float edgeOpacity(IntervalInfo from, IntervalInfo to) {
		if(selectedInterval == null || selectedInterval == from || selectedInterval == to)
			return OPACITY_MAX;
		return OPACITY_MIN;
	}
	
	private void drawInterval(Graphics2D g2, Set<IntervalInfo> adjacentIntervals, IntervalInfo inter) {
		final int x0 = inter.bounds.x, y0 = inter.bounds.y;		
		final Dimension size = inter.bounds.getSize();
		
		float opacity = opacity(adjacentIntervals, inter);
		g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity));
		
		g2.setColor(bgColors[inter.id % bgColors.length]);
		g2.fillRoundRect(x0, y0, size.width, size.height, glm.pointGlyphWidth, glm.pointGlyphHeight);
		
		final int sw = glm.selectionStrokeWidth;
		if(selectedInterval == inter && sw != 0) {
			g2.setStroke(new BasicStroke(sw, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
			g2.setColor(Color.darkGray);
			g2.drawRoundRect(
					x0 - sw / 2, y0 - sw / 2, 
					size.width + sw / 2, size.height + sw / 2, 
					glm.pointGlyphWidth, glm.pointGlyphHeight);
		}

		final double fraction = 0.7;
		int pointX = (int)(glm.pointGlyphWidth * (1.0 - fraction) / 2.0);
		int pointY = (int)(glm.pointGlyphHeight * (1.0 - fraction) / 2.0);
		int pointW = (int)(glm.pointGlyphWidth * fraction);
		int pointH = (int)(glm.pointGlyphHeight * fraction);
		g2.setColor(pointColor);
		g2.fillOval(x0 + pointX, y0 + pointY, pointW, pointH);
		g2.fillOval(x0 + size.width - glm.pointGlyphWidth + pointX, y0 + pointY, pointW, pointH); 
		
		int labelY = (glm.intervalHeight / 2) + (glm.fontMetrics.getAscent() / 2);
		
		g2.setColor(labelColor);
		g2.drawString(
				inter.label, 
				x0 + glm.pointGlyphWidth + glm.labelGapWidth, 
				y0 + labelY);
	}

	private void drawEdges(Graphics2D g2, PointInfo from, Iterable<PointInfo> tos) {
		Point f = glm.center(from);
		for(PointInfo to : tos) {
			Point t = glm.center(to);
			
			float edgeOpacity = edgeOpacity(from.inter, to.inter);
			g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, edgeOpacity)); 
			g2.drawLine(f.x, f.y, t.x, t.y);
		}
	}

}
