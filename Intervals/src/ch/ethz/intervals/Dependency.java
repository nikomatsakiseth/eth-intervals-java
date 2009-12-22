package ch.ethz.intervals;

public interface Dependency {
	
	public Point boundForNewInterval();
	public void addHbToNewInterval(Interval inter);

}
