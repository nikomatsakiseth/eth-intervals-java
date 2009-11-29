package ch.ethz.intervals;

import ch.ethz.intervals.DataRaceException.Role;

class DynamicGuardImpl extends GuardImpl implements DynamicGuard {
	
	PointImpl readInterval;
	PointImpl writeInterval;
	
	@Override
	public void checkRead() throws DataRaceException {
		Current current = Current.get();
		checkRead(current.start, current.end);
	}

	void checkRead(IntervalImpl b) {
		checkRead(b.start, b.end);
	}

	void checkRead(PointImpl start, PointImpl end) {
		synchronized(this) {
			checkHb(Role.WRITE, writeInterval, Role.READ, start);
			
			if(readInterval != null)
				readInterval = readInterval.mutualBound(end);
			else
				readInterval = end; 
		}
	}

	@Override
	public void checkWrite() throws DataRaceException {
		Current current = Current.get();
		checkWrite(current.start, current.end);
	}

	void checkWrite(IntervalImpl b) {
		checkWrite(b.start, b.end);
	}

	void checkWrite(PointImpl start, PointImpl end) {
		PointImpl w, r;
		synchronized(this) {
			r = readInterval;
			w = writeInterval;
			writeInterval = end;
			readInterval = null;
		}
		
		checkHb(Role.READ, r, Role.WRITE, start);
		checkHb(Role.WRITE, w, Role.WRITE, start);
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
