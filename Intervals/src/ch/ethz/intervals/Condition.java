package ch.ethz.intervals;


public abstract class Condition {
	
	/**
	 * Evaluates the condition to a fixed boolean value.  
	 * 
	 * @throws ConditionUnknown if the condition cannot be definitely evaluated to
	 * true or false.  This should only occur when executing reflectively at compile
	 * time.
	 */
	public abstract boolean isTrueFor(RoPoint mr, RoInterval current);
	
	public final String toString() {
		return description();
	}
	
	public abstract String description();
	
	public static final Condition FALSE = new Condition() {
		@Override
		public boolean isTrueFor(RoPoint mr, RoInterval current) {
			return false;
		}

		@Override
		public String description() {
			return "false";
		}
	};
	
	public static final Condition TRUE = new Condition() {
		@Override
		public boolean isTrueFor(RoPoint mr, RoInterval current) {
			return true;
		}

		@Override
		public String description() {
			return "true";
		}
	};
	
	public static final Condition UNKNOWN = new Condition() {
		@Override
		public boolean isTrueFor(RoPoint mr, RoInterval current) {
			throw new ConditionUnknown();
		}

		@Override
		public String description() {
			return "unknown";
		}
	};
	
	static abstract class Binary extends Condition {
		public final Condition left;
		public final Condition right;
		
		protected Binary(Condition left, Condition right) {
			this.left = left;
			this.right = right;
		}
		
		protected abstract boolean combine(boolean l, boolean r);
		
		@Override
		public boolean isTrueFor(RoPoint mr, RoInterval current) {
			return combine(left.isTrueFor(mr, current), right.isTrueFor(mr, current));
		}
	}
	
	public static class And extends Binary {
		protected And(Condition left, Condition right) {
			super(left, right);
		}

		@Override
		protected boolean combine(boolean l, boolean r) {
			return l && r;
		}
		
		@Override
		public String description() {
			return String.format("(%s) && (%s)", left, right);
		}
	}
	
	public static class Or extends Binary {
		protected Or(Condition left, Condition right) {
			super(left, right);
		}

		@Override
		protected boolean combine(boolean l, boolean r) {
			return l || r;
		}		
		
		@Override
		public String description() {
			return String.format("(%s) || (%s)", left, right);
		}
	}
	
	public Condition and(Condition c) {
		return new And(this, c);
	}

	public Condition or(Condition c) {
		return new Or(this, c);
	}

}
