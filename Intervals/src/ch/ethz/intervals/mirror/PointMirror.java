package ch.ethz.intervals.mirror;

import ch.ethz.intervals.Guard;
import ch.ethz.intervals.Point;

/** 
 * Mirror class representing points.  {@link Guard} implementations should
 * use this class in place of {@link Point}.
 */
public interface PointMirror {
	public PointMirror bound();
	public boolean isBoundedBy(PointMirror pnt);
	public boolean hb(PointMirror pnt);
	public void addHb(PointMirror pnt);
}
