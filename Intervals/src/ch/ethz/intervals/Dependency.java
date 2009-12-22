package ch.ethz.intervals;

public interface Dependency {
	
	public Point bound();
	public void addHb(Interval inter);

}
