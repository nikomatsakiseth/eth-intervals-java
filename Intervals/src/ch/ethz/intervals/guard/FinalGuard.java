package ch.ethz.intervals.guard;

import ch.ethz.intervals.Condition;

public final class FinalGuard implements StaticGuard {
	
	public static final FinalGuard Final = new FinalGuard();
	
	private FinalGuard() {
	}

	@Override
	public Condition condReadableBy() {
		return Condition.TRUE;
	}

	@Override
	public Condition condWritableBy() {
		return Condition.FALSE;
	}

	@Override
	public Condition condFinal() {
		return Condition.TRUE;
	}

}
