package ch.ethz.intervals;

public interface DynamicGuard extends Guard {
	/** Checks that it is legal for the current interval to write to data in this guard. */
	public void checkWrite() throws DataRaceException;
	
	/** Checks that it is legal for the current interval to read data in this guard. */
	public void checkRead() throws DataRaceException;
}
