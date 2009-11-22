package ch.ethz.intervals.visualizer;

import javax.swing.DefaultListModel;

import ch.ethz.intervals.visualizer.EventLog.Arrive;
import ch.ethz.intervals.visualizer.EventLog.Edge;
import ch.ethz.intervals.visualizer.EventLog.Lock;
import ch.ethz.intervals.visualizer.EventLog.NewInterval;
import ch.ethz.intervals.visualizer.EventLog.Schedule;

public class UserEventLogVisitor implements EventLogVisitor {
	
	final DefaultListModel listModel;
	final IdMap<Integer> boundsById = new IdMap<Integer>();
	final IdMap<String> labelForId = new IdMap<String>();
	final boolean debug = false;
	
	public UserEventLogVisitor(DefaultListModel listModel) {
		this.listModel = listModel;
	}
	
	private boolean isStart(int id) {
		Integer e = boundsById.get(id);
		return(e != null && e.intValue() == (id + 1));
	}

	@Override
	public void visitArrive(Arrive event, int index)
	{
		if(boundsById.containsKey(event.pointId)) {
			String lbl = labelForId.get(event.pointId);
			String startEnd = (isStart(event.pointId) ? "Start" : "End");
			UserEvent ue = new UserEvent(index, "ARRIVE "+event.pointId+" "+startEnd+"("+lbl+")");
			listModel.addElement(ue);
		}
	}

	@Override
	public void visitEdge(Edge event, int index)
	{
		if(debug)
			if(boundsById.containsKey(event.fromPointId) && boundsById.containsKey(event.toPointId)) {
				UserEvent ue = new UserEvent(index, "EDGE "+event.fromPointId+"->"+event.toPointId);
				listModel.addElement(ue);
			}
	}

	@Override
	public void visitLock(Lock event, int index)
	{
	}

	@Override
	public void visitNewInterval(NewInterval event, int index)
	{
		assert event.endId == event.startId + 1;
		boundsById.set(event.startId, event.endId);
		boundsById.set(event.endId, event.endBoundId);
	}

	@Override
	public void visitSchedule(Schedule event, int index)
	{
		Integer endId = boundsById.get(event.startPointId);
		if(endId != null) {
			UserEvent ue = new UserEvent(index, "NEW "+event.taskDescr+
					" ("+event.startPointId+"-"+endId+")");
			listModel.addElement(ue);
			
			labelForId.set(event.startPointId, event.taskDescr);
			labelForId.set(endId, event.taskDescr);
		}		
	}

}
