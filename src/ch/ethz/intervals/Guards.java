package ch.ethz.intervals;

public class Guards {

	public static Guard newGuard() {
		return new GuardImpl();
	}
	
	public static DynamicGuard newDynamicGuard() {
		return new DynamicGuardImpl();
	}

}
