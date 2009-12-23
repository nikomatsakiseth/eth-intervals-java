package ch.ethz.intervals.visualizer;

/**
 * Used for constructing and laying out graphs.
 * 
 * A little bit "multi-purpose" but who wants to go through the trouble
 * of defining two visitors?
 */
public interface EventLogVisitor {
	
	public void visitArrive(EventLog.Arrive event, int index);
	public void visitEdge(EventLog.Edge event, int index);
	public void visitLock(EventLog.AddLock event, int index);
	public void visitNewInterval(EventLog.NewInterval event, int index);
	public void visitSchedule(EventLog.Schedule event, int index);

}
