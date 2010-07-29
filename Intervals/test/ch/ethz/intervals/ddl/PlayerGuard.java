package ch.ethz.intervals.ddl;

import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;
import ch.ethz.intervals.guard.Guard;

public class PlayerGuard 
implements Guard 
{
	public final GameBoard board;
	public final Player player;
	
	public PlayerGuard(GameBoard board, Player player) {
		this.board = board;
		this.player = player;
	}

	@Override
	public RuntimeException checkWritable(RoPoint mr, RoInterval current) {
		return player.lock.checkWritable(mr, current);
	}

	@Override
	public RuntimeException checkReadable(RoPoint mr, RoInterval current) {
		return player.lock.checkReadable(mr, current);
	}

	@Override
	public RuntimeException ensuresFinal(RoPoint mr, RoInterval current) {
		return player.lock.ensuresFinal(mr, current);
	}

	@Override
	public RuntimeException checkLockable(RoPoint acq, RoInterval interval, RoLock lock) {
		if(lock != player.lock)
			return new RuntimeException("Not player's lock");
		if(player.node.isLeaf())
			return new RuntimeException("Do not lock player when in leaf node");
		if(!acq.releasesLock(player.node.lock, null))
			return new RuntimeException("Must lock gameboard to acquire player lock");
		return null;
	}

}
