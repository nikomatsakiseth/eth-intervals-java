package ch.ethz.intervals.visualizer;


public class EventLog {
	
	public abstract static class Event {
		public abstract void accept(EventLogVisitor doVisitor, int index);
	}
	
	public static class NewInterval extends Event {
		public final int currentId;
		public final int startId;
		public final int endId;
		public final int endBoundId;

		public NewInterval(
				int currentId,
				int startId,
				int endId, 
				int endBoundId) {
			this.currentId = currentId;
			this.startId = startId;
			this.endId = endId;
			this.endBoundId = endBoundId;
		}

		@Override
		public String toString() {
			return String.format("NEW_INTERVAL %s %s %s %s", currentId, startId, endId, endBoundId); 
		}

		@Override
		public void accept(EventLogVisitor visitor, int index) {
			visitor.visitNewInterval(this, index);
		}
	}

	public static class Edge extends Event {
		public final int fromPointId;
		public final int toPointId;
		
		public Edge(int fromPointId, int toPointId) {
			this.fromPointId = fromPointId;
			this.toPointId = toPointId;
		}

		@Override
		public String toString() {
			return String.format("EDGE %s %s", fromPointId, toPointId);
		}

		@Override
		public void accept(EventLogVisitor visitor, int index) {
			visitor.visitEdge(this, index);
		}
	}

	public static class Lock extends Event {
		public final int startPointId;
		public final int lockId;
		
		public Lock(int startPointId, int lockId) {
			this.startPointId = startPointId;
			this.lockId = lockId;
		}
		
		@Override
		public String toString() {
			return String.format("LOCK %s %s", startPointId, lockId);
		}

		@Override
		public void accept(EventLogVisitor visitor, int index) {
			visitor.visitLock(this, index);
		}
	}
	
	public static class Schedule extends Event {
		public final int startPointId;
		public final String taskDescr;
		
		public Schedule(int startPointId, String taskDescr) {
			this.startPointId = startPointId;
			this.taskDescr = taskDescr;
		}
		
		@Override
		public String toString() {
			return String.format("SCHEDULE %s %s", startPointId, taskDescr);
		}

		@Override
		public void accept(EventLogVisitor visitor, int index) {
			visitor.visitSchedule(this, index);
		}
	}

	public static class Arrive extends Event {
		public final int pointId;

		public Arrive(int pointId) {
			this.pointId = pointId;
		}

		@Override
		public String toString() {
			return String.format("ARRIVE %s", pointId);
		}

		@Override
		public void accept(EventLogVisitor visitor, int index) {
			visitor.visitArrive(this, index);
		}
	}

	/** 
	 * Special event class that is used as a sentinel message from producer
	 * to consumer. 
	 */
	private static class StopLogging extends Event {

		@Override
		public void accept(EventLogVisitor doVisitor, int index) {
		}

		public String toString() {
			return "STOP";
		}
		
	}
	
	public static final Event STOP_LOGGING = new StopLogging();

}
