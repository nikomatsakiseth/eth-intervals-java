package ch.ethz.intervals.ddl;

import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.quals.GuardedBy;

public class Player {
	public final PlayerGuard guard;
	public final String name;
	public final Lock lock;
	public final int size;
	
	@GuardedBy("guard")
	public GameBoard node = null;
	
	@GuardedBy("guard")
	public int x = 0, y = 0;
	
	public Player(
			GameBoard board,
			String name,
			int size
	) {
		this.name = name;
		this.lock = Intervals.lock(String.format("P-%s", name));
		this.size = size;
		this.guard = new PlayerGuard(board, this);
	}

	public void checkReadable() {
		Intervals.checkReadable(guard);
	}

	public void checkWritable() {
		Intervals.checkWritable(guard);
	}
	
}
