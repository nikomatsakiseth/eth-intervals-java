package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.SPECULATIVE;
import ch.ethz.intervals.DataRaceException.Role;

class DynamicGuardImpl extends GuardImpl implements DynamicGuard {
	
	Point readInterval;
	Point writeInterval;
	
	@Override
	public void checkRead() throws DataRaceException {
		Current current = Current.get();
		checkRead(current.start, current.end);
	}

	void checkRead(Interval b) {
		checkRead(b.start, b.end);
	}

	void checkRead(Point start, Point end) {
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

	void checkWrite(Interval b) {
		checkWrite(b.start, b.end);
	}

	void checkWrite(Point start, Point end) {
		Point w, r;
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
			Point fromPoint,
			Role toRole,
			Point toPoint)
	{
		if(fromPoint != null)
			if(!fromPoint.hb(toPoint, SPECULATIVE))
				throw new DataRaceException(this, fromRole, fromPoint, toRole, toPoint);		
	}

}
