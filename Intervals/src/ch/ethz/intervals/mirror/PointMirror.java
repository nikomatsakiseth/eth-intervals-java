package ch.ethz.intervals.mirror;

import ch.ethz.intervals.Point;
import ch.ethz.intervals.guard.Guard;

/** 
 * Mirror class representing points.  {@link Guard} implementations should
 * use this class in place of {@link Point}.
 */
public interface PointMirror {
	public PointMirror bound();
	public boolean isBoundedBy(PointMirror pnt);
	public boolean hb(PointMirror pnt);
	public boolean hbeq(PointMirror pnt);
	public void addHb(PointMirror pnt);
}
