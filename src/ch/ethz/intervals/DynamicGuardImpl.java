package ch.ethz.intervals;

import ch.ethz.intervals.DataRaceException.Role;

class DynamicGuardImpl extends GuardImpl implements DynamicGuard {
	
	PointImpl readInterval;
	PointImpl writeInterval;
	
	@Override
	public void checkRead() throws DataRaceException {
		IntervalImpl<?> currentInterval = Intervals.currentInterval.get();		
		checkRead(currentInterval);
	}

	void checkRead(IntervalImpl<?> currentInterval) {
		synchronized(this) {
			checkHb(Role.WRITE, writeInterval, Role.READ, currentInterval.start);
			
			if(readInterval != null)
				readInterval = readInterval.mutualBound(currentInterval.end);
			else
				readInterval = currentInterval.end; 
		}
	}

	@Override
	public void checkWrite() throws DataRaceException {
		IntervalImpl<?> currentInterval = Intervals.currentInterval.get();
		checkWrite(currentInterval);
	}

	void checkWrite(IntervalImpl<?> currentInterval) {
		PointImpl w, r;
		synchronized(this) {
			r = readInterval;
			w = writeInterval;
			writeInterval = currentInterval.end;
			readInterval = null;
		}
		
		checkHb(Role.READ, r, Role.WRITE, currentInterval.start);
		checkHb(Role.WRITE, w, Role.WRITE, currentInterval.start);
	}
	
	private void checkHb(
			Role fromRole,
			PointImpl fromPoint,
			Role toRole,
			PointImpl toPoint)
	{
		if(fromPoint != null)
			if(!fromPoint.hb(toPoint, false))
				throw new DataRaceException(this, fromRole, fromPoint, toRole, toPoint);		
	}

}
