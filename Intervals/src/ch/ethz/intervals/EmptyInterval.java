package ch.ethz.intervals;

public class EmptyInterval extends Interval {
	
	private final String name;
	
	EmptyInterval(Interval parent, Point start, Point end, String name) {
		super(parent, start, end);
		this.name = name;
	}

	public EmptyInterval(Dependency dep, String name) {
		super(dep);
		this.name = name;
	}
	
	@Override
	public String toString() {
		return name;
	}

	@Override
	protected void run() {
	}

}
