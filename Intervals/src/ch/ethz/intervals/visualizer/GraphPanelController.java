package ch.ethz.intervals.visualizer;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.List;

import javax.swing.JScrollPane;
import javax.swing.SwingWorker;

import ch.ethz.intervals.visualizer.EventLog.Arrive;
import ch.ethz.intervals.visualizer.EventLog.Edge;
import ch.ethz.intervals.visualizer.EventLog.AddLock;
import ch.ethz.intervals.visualizer.EventLog.NewInterval;
import ch.ethz.intervals.visualizer.EventLog.Schedule;

public class GraphPanelController 
implements MouseListener
{	
	private final JScrollPane scrollPane;
	private final GraphPanel graphPanel;
	private final InfoPanel infoPanel;
	private SwingWorker<?, ?> scrollWorker;	
	public int currentTime = -1; // adjust by the Window
	public final EventLogVisitor selectionDoVisitor = new SelectionUpdater();

	public GraphPanelController(
			JScrollPane scrollPane, 
			GraphPanel graphPanel,
			InfoPanel infoPanel) 
	{
		this.scrollPane = scrollPane;
		this.graphPanel = graphPanel;
		this.infoPanel = infoPanel;
		this.graphPanel.addMouseListener(this);
	}
	
	@Override
	public void mouseClicked(MouseEvent e) {
		int button = e.getButton();
		if(button == MouseEvent.BUTTON1) {
			// Identify which interval, if any, was clicked.
			IntervalInfo intervalPanel = graphPanel.intervalAt(e.getPoint());
			selectInterval(intervalPanel);
		}
	}

	public void selectInterval(IntervalInfo intervalPanel) {
		graphPanel.setSelectedInterval(intervalPanel);		
		infoPanel.loadInfoFor(intervalPanel);
		
		if(intervalPanel != null) {		
			Rectangle viewBounds = scrollPane.getViewport().getBounds();
			Rectangle graphBounds = graphPanel.getBounds();
			Rectangle bounds = intervalPanel.bounds;
			
			int x = (int)Math.max(bounds.getCenterX() - viewBounds.getWidth() / 2.0, 0.0);
			int y = (int)Math.max(bounds.getCenterY() - viewBounds.getHeight() / 2.0, 0.0);
			
			if(x + viewBounds.width > graphBounds.width)
				x = Math.max(graphBounds.width - viewBounds.width, 0);
			
			if(y + viewBounds.height > graphBounds.height)
				y = Math.max(graphBounds.height - viewBounds.height, 0);
			
			scrollTo(x, y);
		}
				
		scrollPane.repaint();
	}

	@Override
	public void mouseEntered(MouseEvent e) {
	}

	@Override
	public void mouseExited(MouseEvent e) {
	}

	@Override
	public void mousePressed(MouseEvent e) {
	}

	@Override
	public void mouseReleased(MouseEvent e) {
	}
	
	private void scrollTo(int x, int y) {
		if(scrollWorker != null)
			scrollWorker.cancel(true);		
		scrollWorker = new ScrollWorker(x, y);
		scrollWorker.execute();
	}

	class ScrollWorker extends SwingWorker<Void, Point> {
		
		final private Point startPoint, finalPoint;
		final private int steps = 10;
		final private long sleepMillis = 50;

		public ScrollWorker(int finalX, int finalY) {
			startPoint = scrollPane.getViewport().getViewPosition();
			finalPoint = new Point(finalX, finalY);
		}

		@Override
		protected Void doInBackground() throws Exception {
			double incrX = ((double)(finalPoint.x - startPoint.x)) / steps;
			double incrY = ((double)(finalPoint.y - startPoint.y)) / steps;
			double x = startPoint.x, y = startPoint.y;
			for(int i = 0; i < steps && !isCancelled(); i++) {
				x += incrX;
				y += incrY;
				publish(new Point((int)x, (int)y));
				Thread.sleep(sleepMillis);
			}
			if(!isCancelled())
				publish(finalPoint);
			return null;
		}

		@Override
		protected void process(List<Point> chunks) {
			super.process(chunks);
			
			Point p = chunks.get(chunks.size() - 1);
			scrollPane.getViewport().setViewPosition(p);
//			scrollPane.getViewport().repaint(); // this seems to be necessary...
		}

		@Override
		protected void done() {
			super.done();
			
			scrollWorker = null;
		}
		
	}
	
	class SelectionUpdater implements EventLogVisitor {

		@Override
		public void visitArrive(Arrive event, int index) {
			PointInfo pnt = graphPanel.pointById(event.pointId);
			if(pnt != null)
				selectInterval(pnt.inter);
		}

		@Override
		public void visitEdge(Edge event, int index) {
		}

		@Override
		public void visitLock(AddLock event, int index) {
		}

		@Override
		public void visitNewInterval(NewInterval event, int index) {
		}

		@Override
		public void visitSchedule(Schedule event, int index) {
			PointInfo pnt = graphPanel.pointById(event.startPointId);
			if(pnt != null)
				selectInterval(pnt.inter);
		}
		
	}
	
}
