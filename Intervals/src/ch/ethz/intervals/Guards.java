package ch.ethz.intervals;

/** Static methods for working with guards. */
public class Guards {

	public static Guard newGuard() {
		return new GuardImpl();
	}
	
	public static DynamicGuard newDynamicGuard() {
		return new DynamicGuardImpl();
	}

}
