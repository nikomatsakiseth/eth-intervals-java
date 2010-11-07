package ch.ethz.intervals.guard;

import ch.ethz.intervals.Condition;

public interface Guard {
	public Condition condReadableBy();
	public Condition condWritableBy();
	public Condition condFinal();
}
