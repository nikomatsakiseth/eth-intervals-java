package ch.ethz.intervals.visualizer;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

public class ConflictMap {
	
	final int COL_GRID_SIZE;
	final int ROW_GRID_SIZE;
	
	private List<List<List<IntervalInfo>>> grid = new ArrayList<List<List<IntervalInfo>>>();
	
	public ConflictMap(GraphLayoutMetrics glm) {
		COL_GRID_SIZE = 100;
		ROW_GRID_SIZE = glm.rowHeight();
	}
	
	public Dimension computeSize() {
		int rows = grid.size();
		int cols = 0;
		
		for(int y = 0; y < rows; y++)
			cols = Math.max(cols, grid.get(y).size());
		
		return new Dimension(cols * COL_GRID_SIZE, rows * ROW_GRID_SIZE);				
	}
	
	public IntervalInfo rightmost(final int yGrid) {
		if(yGrid >= grid.size())
			return null;
		
		List<List<IntervalInfo>> rowData = grid.get(yGrid);
		if(rowData.isEmpty())
			return null;
		
		List<IntervalInfo> finalCol = rowData.get(rowData.size() - 1);
		IntervalInfo rightmost = null;
		for(IntervalInfo intervalInfo : finalCol) {
			Rectangle bounds = intervalInfo.bounds;
			if(rightmost == null)
				rightmost = intervalInfo;
			else if (bounds.getMaxX() > rightmost.bounds.getMaxX())
				rightmost = intervalInfo;
		}		
		return rightmost;
	}
	
	/** Returns all intervals whose bounds intersect {@code bounds} */
	public List<IntervalInfo> getAllIntersecting(Rectangle bounds) {
		List<IntervalInfo> result = new ArrayList<IntervalInfo>();
		
		int yMinGrid = bounds.y / ROW_GRID_SIZE;
		int yMaxGrid = (bounds.y + bounds.height) / ROW_GRID_SIZE;
		
		int xMinGrid = bounds.x / COL_GRID_SIZE;
		int xMaxGrid = (bounds.x + bounds.width) / COL_GRID_SIZE;
		
		for(int y = yMinGrid; y <= yMaxGrid; y++) {
			if(y >= grid.size())
				break;
			List<List<IntervalInfo>> rowData = grid.get(y);
			
			for(int x = xMinGrid; x <= xMaxGrid; x++) {
				if(x >= rowData.size())
					break;

				List<IntervalInfo> colData = rowData.get(x);
				for(IntervalInfo inter : colData) {
					if(inter.bounds.intersects(bounds))
						result.add(inter);
				}
			}
		}
		
		return result;
	}

	public IntervalInfo get(Point p) {
		int yGrid = p.y / ROW_GRID_SIZE;
		if(yGrid >= grid.size())
			return null;
		
		List<List<IntervalInfo>> rowData = grid.get(yGrid);
		int xGrid = p.x / COL_GRID_SIZE;
		if(xGrid >= rowData.size())
			return null;
		
		List<IntervalInfo> colData = rowData.get(xGrid);
		for(IntervalInfo panel : colData) {
			if(panel.bounds.contains(p))
				return panel;
		}
		
		return null;
	}

	public void add(IntervalInfo panel) {
		
		Rectangle bounds = panel.bounds;
		
		int yMinGrid = bounds.y / ROW_GRID_SIZE;
		int yMaxGrid = (bounds.y + bounds.height) / ROW_GRID_SIZE;
		
		int xMinGrid = bounds.x / COL_GRID_SIZE;
		int xMaxGrid = (bounds.x + bounds.width) / COL_GRID_SIZE;
		
		for(int y = yMinGrid; y <= yMaxGrid; y++) {
			List<List<IntervalInfo>> rowData = createRowDataIfNecessary(y);
			for(int x = xMinGrid; x <= xMaxGrid; x++) {
				List<IntervalInfo> colData = createColDataIfNecessary(rowData, x);
//				System.err.printf("add(%s): x=%d y=%d\n", panel, x, y);
				colData.add(panel);
			}			
		}
		
	}

	public void remove(IntervalInfo panel) {
		
		Rectangle bounds = panel.bounds;
		
		int yMinGrid = bounds.y / ROW_GRID_SIZE;
		int yMaxGrid = (bounds.y + bounds.height) / ROW_GRID_SIZE;
		
		int xMinGrid = bounds.x / COL_GRID_SIZE;
		int xMaxGrid = (bounds.x + bounds.width) / COL_GRID_SIZE;
		
		boolean anyEmpty = false;
		for(int y = yMinGrid; y <= yMaxGrid; y++) {
			List<List<IntervalInfo>> rowData = grid.get(y);
			for(int x = xMinGrid; x <= xMaxGrid; x++) {
//				System.err.printf("remove(%s): x=%d y=%d\n", panel, x, y);
				List<IntervalInfo> colData = rowData.get(x);
				colData.remove(panel);
				if(colData.isEmpty())
					anyEmpty = true; 
			}			
		}
		
		if(anyEmpty)
			shrink(yMinGrid, yMaxGrid);
		
	}
	
	private void shrink(int yMinGrid, int yMaxGrid) {
		// First shrink any columns that may have changed:
		for(int y = yMaxGrid; y >= yMinGrid; y--) {
			List<List<IntervalInfo>> rowData = grid.get(y);
			for(int x = rowData.size() - 1; x >= 0; x--) {
				List<IntervalInfo> finalCol = rowData.get(x);
				if(!finalCol.isEmpty())
					break;
//				System.err.printf("shrink(): remove x=%d y=%d\n", x, y);
				rowData.remove(x);
			}
		}
		
		// Now shrink rows if possible:
		for(int y = grid.size() - 1; y >= 0; y--) {
			List<List<IntervalInfo>> rowData = grid.get(y);			
			if(!rowData.isEmpty())
				break;
			grid.remove(y);
//			System.err.printf("shrink(): remove y=%d\n", y);
		}
	}
	
	private List<List<IntervalInfo>> createRowDataIfNecessary(int y) {
		while(y >= grid.size())
			grid.add(new ArrayList<List<IntervalInfo>>());
		return grid.get(y);
	}

	private List<IntervalInfo> createColDataIfNecessary(List<List<IntervalInfo>> rowData, int x) {
		while(x >= rowData.size())
			rowData.add(new ArrayList<IntervalInfo>());
		return rowData.get(x);
	}

}
