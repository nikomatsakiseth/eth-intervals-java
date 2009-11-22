package ch.ethz.intervals.visualizer;

import java.util.List;

public interface LayerOuter {

	/**
	 * Indicates that we should re-layout the given
	 * list of {@code pnts}, and the current time is {@code time}.
	 * Re-laying out {@code pnts} may entail adjust the
	 * layout of other intervals as well.
	 */	
	public void layout(int time, List<PointInfo> pnts);
	
}
