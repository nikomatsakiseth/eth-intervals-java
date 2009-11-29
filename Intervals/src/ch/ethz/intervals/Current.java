package ch.ethz.intervals;

class Current {
	
	private static Current root = new Current(null, null, Intervals.ROOT_END);
	private static ThreadLocal<Current> local = new ThreadLocal<Current>() {

		@Override
		protected Current initialValue() {
			return root;
		}
		
	};
	
	public static Current get() {
		return local.get();
	}
	
	public static Current push(PointImpl start, PointImpl end) {
		Current c = get();
		Current n = new Current(c, start, end);
		local.set(n);
		return n;
	}
	
	public Current prev;
	public PointImpl start;
	public PointImpl end;

	public Current(Current prev, PointImpl start, PointImpl end) {
		this.prev = prev;
		this.start = start;
		this.end = end;
	}	

	public void pop() {
		local.set(prev);
	}
	
}
