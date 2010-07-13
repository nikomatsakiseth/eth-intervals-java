package ch.ethz.intervals.impl;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import pcollections.PSet;
import ch.ethz.intervals.impl.IntervalImpl.State;
import ch.ethz.intervals.impl.ThreadPool.Medallion;
import ch.ethz.intervals.impl.ThreadPool.WorkItem;
import ch.ethz.intervals.impl.ThreadPool.Worker;

import com.smallcultfollowing.lathos.CustomOutput;
import com.smallcultfollowing.lathos.JettyLathosServer;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;
import com.smallcultfollowing.lathos.PageContent;

/**
 * Used for debugging the execution of intervals.
 * Can be enabled by setting the 
 * property IntervalsDebug to true.
 * 
 * This Debug object itself is just a Lathos page.
 * To actually view debug information, you must
 * create a {@link LathosServer} and add {@link Debug#debug}
 * as a top-level page.
 * 
 * The basic implementation is a kind of actor model.
 * The various intervals post events into a central queue
 * (we could later change this into a set of per-worker queues).
 * A separate thread reads that queue and updates the data structures.
 * This allows us to do some fairly complex tracking.
 */
public class Debug 
implements Page 
{
	/**
	 * If this variable is non-zero, then debugging is enabled
	 * and a JettyLathosServer is automatically started. 
	 * This is very useful for unit tests and the like.*/
	private static final int STATIC_PORT = 0; 
	
	/**
	 * Controls whether debugging is enabled.   
	 * Initialized from the property {@code IntervalsDebug}.
	 * Defaults to {@code false}. */
	public static final boolean ENABLED = 
		(STATIC_PORT != 0) || Boolean.parseBoolean(System.getProperty("IntervalsDebug", "false"));

	/** 
	 * Throw out events if we have more than this amount, unless the relevant
	 * point has not yet occurred. 
	 * 
	 * Initialized from the property {@code IntervalsDebug.Memory1}.
	 * Defaults to {@code 10000}. */
	public static final int MEMORY1 = 
		Integer.parseInt(System.getProperty("IntervalsDebug.Memory1", "10000"));
	
	/** 
	 * Unconditionally remove events when we have accumulated this amount. 
	 * 
	 * Initialized from the property {@code IntervalsDebug.Memory2}.
	 * Defaults to {@link #MEMORY1} {@code * 10}. */
	public static final int MEMORY2 = 
		Integer.parseInt(System.getProperty("IntervalsDebug.Memory2", Integer.toString(MEMORY1 * 10)));
	
	/** 
	 * Maximum capacity of the event log queue.  This prevents the worker threads 
	 * from swamping the update thread with events.
	 * 
	 * Initialized from the property {@code IntervalsDebug.QueueCapacity}.
	 * Defaults to {@link #MEMORY1}. */
	public static final int QUEUE_CAPACITY = 
		Integer.parseInt(System.getProperty("IntervalsDebug.QueueCapacity", Integer.toString(MEMORY1)));
	
	/**
	 * Singleton instance. */
	public static final Debug debug = new Debug();
	
	static {
		if(STATIC_PORT != 0) {
			LathosServer server;
			try {
				server = JettyLathosServer.start(STATIC_PORT);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
			server.registerPage(debug);
		}
	}
	
	private Debug() {
		updateThread = new UpdateThread();
		updateThread.start();
	}
	
	// Update thread:
	// --------------

	private BlockingQueue<Event> queue = new ArrayBlockingQueue<Debug.Event>(QUEUE_CAPACITY, true);
	
	private void putQueue(Event event) {
		try {
			queue.put(event);
		} catch (InterruptedException e) {}
	}

	private class UpdateThread extends Thread {
		
		public UpdateThread() {
			super("Interval Debug Thread");
			setDaemon(true);
		}
		
		@Override
		public void run() {
			try {
				while(true) {
					Event event = queue.take();
					synchronized(Debug.this) {
						event.post(Debug.this);
					}
				}
			} catch (InterruptedException e) {
			}
		}
		
	}
	
	private final UpdateThread updateThread;
	
	// Interval data structures:
	// -------------------------
	//
	// Accessed only while holding lock on the RefTracker.
	// Generally this is done during the update thread.
	
	private class EventList {
		int size;
		Event first;
		Event last;
	}
	
	private abstract static class Event implements CustomOutput {
		final PointImpl relevantTo;
		long id;
		Event prevAll, nextAll;
		Event prevObject, nextObject;
		
		public Event(PointImpl relevant) {
			this.relevantTo = relevant;
		}

		/** 
		 * Invoked from the update thread while 
		 * holding the RefTracker lock. */
		public void post(Debug debug) {
			debug.addEvent(this);
		}
		
		/**
		 * Returns true if this event pertains to something 
		 * which has already happened and is in the past.
		 * If there are too many events, irrelevant events are
		 * purged first. */
		public boolean irrelevant() {
			if(relevantTo != null)
				// Already occurred == irrelevant
				return relevantTo.didOccur(); // racy, but so what?
			
			return true;
		}

		public void outputComments(Output out) 
		throws IOException {
		}
	}
	
	private final Map<PointImpl, EventList> perObjectLists = new HashMap<PointImpl, EventList>(); 
	private final Map<RefEvent, AddRefEvent> unmatchedAddRefEvents = new HashMap<RefEvent, AddRefEvent>();
	private EventList allEvents = new EventList();
	private long counter = 0;
	
	private void addEvent(Event event) {
		event.id = counter;
		counter += 1;
		
		if(event.relevantTo != null) {
			EventList list = perObjectLists.get(event.relevantTo);
			if(list == null) {
				perObjectLists.put(event.relevantTo, (list = new EventList()));
				list.first = list.last = event;
			} else {
				assert list.first != null;
				list.last.nextObject = event;
				event.prevObject = list.last;
				list.last = event;
			}
			list.size++;
		}

		allEvents.size++;
		if(allEvents.first == null) {
			allEvents.first = allEvents.last = event;
		} else {
			allEvents.last.nextAll = event;
			event.prevAll = allEvents.last;
			allEvents.last = event;
			
			purgeOldEvents();
		}
	}
	
	private void purgeOldEvents() {
		Event victim = allEvents.first;
		
		// Never allow more than MEMORY2 events:
		while(allEvents.size > MEMORY2) {
			victim = purgeEvent(victim);
		}
		
		// Allow more than MEMORY1 events only if they are still relevant:
		while(victim != null && allEvents.size > MEMORY1) {
			if(victim.irrelevant()) {
				victim = purgeEvent(victim);
			} else {
				victim = victim.nextAll;
			}
		}
	}

	private Event purgeEvent(Event victim) {
		
		// Remove from per-object list:
		if(victim.relevantTo != null) {
			EventList objectList = perObjectLists.get(victim.relevantTo);

			if(objectList.first == victim) {
				assert victim.prevObject == null;
				objectList.first = victim.nextObject;
			} else {
				victim.prevObject.nextObject = victim.nextObject;
			}

			if(victim.nextObject != null) {
				victim.nextObject.prevObject = victim.prevObject;
			} else {
				assert victim.nextObject == null;
				objectList.last = victim.prevObject;
			}
			
			victim.prevObject = null;
			victim.nextObject = null;
			objectList.size--;
			
			if(objectList.size == 0)
				perObjectLists.remove(victim.relevantTo);
		}
		
		if(allEvents.first == victim) {
			assert victim.prevAll == null;
			allEvents.first = victim.nextAll;
		} else {
			victim.prevAll.nextAll = victim.nextAll;
		}

		if(allEvents.last == victim) {
			assert victim.nextAll == null;
			allEvents.last = victim.prevAll;
		} else {
			victim.nextAll.prevAll = victim.prevAll;
		}
		
		Event nextAll = victim.nextAll;
		victim.prevAll = null;
		victim.nextAll = null;
		
		allEvents.size--;

		return nextAll;
	}

	// Posting routines:
	// -----------------
	
	abstract static class RefEvent extends Event {
		final Object from;
		long matchedWith = -1; 
		
		public RefEvent(PointImpl point, Object from) {
			super(point);
			this.from = from;
		}
		
		public boolean isMatched() {
			return matchedWith != -1;
		}
		
		@Override
		public boolean irrelevant() {
			return super.irrelevant() || isMatched();
		}

		@Override
		public void outputComments(Output out) 
		throws IOException {
			if(!isMatched()) {
				out.startBold();
				out.outputText("Unmatched.");
				out.endBold();
			} else {
				out.outputText("Matched to ");
				out.outputText(Long.toString(matchedWith));
			}
		}

		@Override 
		public boolean equals(Object object) {
			if(object == this) {
				return true;				
			} else if(object instanceof RefEvent) {
				// HACK-- Any two ref events (either add or dec)
				// compare equal if the point/from are the same.
				// This avoids the need to allocate another 
				// object when we find unmatched adds/decs.
				RefEvent ref = (RefEvent) object;
				return (relevantTo == ref.relevantTo && from == ref.from);
			} else return false;
		}
		
		@Override 
		public int hashCode() {
			return System.identityHashCode(relevantTo) ^ System.identityHashCode(from);
		}
	}
	
	static class AddRefEvent extends RefEvent {
		final int newValue;
		
		// If this field is null or points to an AddRefEvent, this
		// add ref is not yet matched against a DecRef.  Set to
		// null once we found a match.
		AddRefEvent nextUnmatched;
		
		public AddRefEvent(PointImpl point, Object from, int newValue) {
			super(point, from);
			this.newValue = newValue;
		}
		
		@Override
		public void post(Debug debug) {
			super.post(debug);
			
			nextUnmatched = debug.unmatchedAddRefEvents.put(this, this);
		}
		
		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("AddRef(");
			output.outputObject(relevantTo);
			output.outputText(" <- ");
			output.outputObject(from);
			output.outputText(" = ");
			output.outputText(Integer.toString(newValue));
			output.outputText(")");
		}
	}
	
	public void postAddRef(PointImpl point, Object from, int newValue) {
		if(ENABLED) {
			putQueue(new AddRefEvent(point, from, newValue));
		}
	}

	static class DecRefEvent extends RefEvent {
		final int newValue;
		
		public DecRefEvent(PointImpl point, Object from, int newValue) {
			super(point, from);
			this.newValue = newValue;
		}
		
		@Override
		public void post(Debug debug) {
			super.post(debug);
			
			AddRefEvent match = debug.unmatchedAddRefEvents.get(this);
			if(match != null) {
				if(match.nextUnmatched == null)
					debug.unmatchedAddRefEvents.remove(match);
				else
					debug.unmatchedAddRefEvents.put(
							match.nextUnmatched, 
							(AddRefEvent) match.nextUnmatched);
				
				matchedWith = match.id;
				match.matchedWith = id;
			} else {
				// This really should not happen.
			}
		}
		
		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("DecRef(");
			output.outputObject(relevantTo);
			output.outputText(" <- ");
			output.outputObject(from);
			output.outputText(" = ");
			output.outputText(Integer.toString(newValue));
			output.outputText(")");
		}
	}
	
	public void postDecRef(PointImpl point, RefManipulator from, int newValue) {
		if(ENABLED) {
			putQueue(new DecRefEvent(point, from, newValue));
		}
	}
	
	static class JoinEvent extends Event {
		public final IntervalImpl joiner;
		public final Worker worker;
		
		public JoinEvent(PointImpl joinedPoint, IntervalImpl joiner, Worker worker) {
			super(joinedPoint);
			this.joiner = joiner;
			this.worker = worker;
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("Joined(");
			output.outputObject(relevantTo);
			output.outputText(" by ");
			output.outputObject(joiner);
			output.outputText(" worker ");
			output.outputObject(worker);
			output.outputText(")");
		}

	}

	public void postJoin(PointImpl joinedPoint, IntervalImpl joiner, Worker worker) {
		if(ENABLED) {
			putQueue(new JoinEvent(joinedPoint, joiner, worker));
		}
	}

	static class OccurEvent extends Event {
		public OccurEvent(PointImpl point) {
			super(point);
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("Occur(");
			output.outputObject(relevantTo);
			output.outputText(")");
		}

	}
	
	public void postOccur(PointImpl point) {
		if(ENABLED) {
			putQueue(new OccurEvent(point));
		}
	}
	
	static class ScheduleEvent extends Event {
		final IntervalImpl scheduled;
		final IntervalImpl scheduledBy;
		
		public ScheduleEvent(IntervalImpl scheduled, IntervalImpl scheduledBy) {
			super(scheduled.start);
			this.scheduled = scheduled;
			this.scheduledBy = scheduledBy;
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("Schedule(");
			output.outputObject(scheduled);
			output.outputText(" by ");
			output.outputObject(scheduledBy);
			output.outputText(")");
		}

	}
	
	public void postSchedule(IntervalImpl scheduled, IntervalImpl scheduledBy) {
		if(ENABLED) {
			putQueue(new ScheduleEvent(scheduled, scheduledBy));
		}
	}
	
	static class TransitionEvent extends Event {
		final IntervalImpl interval;
		final State oldState;
		final State newState;
		
		public TransitionEvent(
				IntervalImpl interval,
				State oldState, 
				State newState) 
		{
			super(interval.end);
			this.interval = interval;
			this.oldState = oldState;
			this.newState = newState;
		}
		
		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("Transition(");
			output.outputObject(interval);
			output.outputText(" from ");
			output.outputObject(oldState);
			output.outputText(" to ");
			output.outputObject(newState);
			output.outputText(")");
		}
	}
	
	public void postTransition(
			IntervalImpl intervalImpl, 
			State state,
			State newState) 
	{
		if(ENABLED) {
			putQueue(new TransitionEvent(intervalImpl, state, newState));
		}
	}
	
	static class VertExceptionEvent extends Event {
		final IntervalImpl interval;
		final int totalCount;
		
		public VertExceptionEvent(
				IntervalImpl interval,
				int totalCount) 
		{
			super(interval.end);
			this.interval = interval;
			this.totalCount = totalCount;
		}
		
		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("VertException(");
			output.outputObject(interval);
			output.outputText(" total ");
			output.outputText(Integer.toString(totalCount));
			output.outputText(")");
		}
	}

	public void postVertException(
			IntervalImpl intervalImpl, 
			Throwable thr,
			PSet<Throwable> vertExceptions) 
	{
		if(ENABLED) {
			putQueue(new VertExceptionEvent(intervalImpl, vertExceptions.size()));
		}
	}

	static class LockEvent extends Event {
		final String kind;
		final LockBase lock;
		final LockList acq;
		
		public LockEvent(
				String kind, 
				LockBase lock,
				LockList acq) 
		{
			super((acq != null ? acq.inter.end : null));
			this.kind = kind;
			this.lock = lock;
			this.acq = acq;
		}
		
		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText(kind);
			output.outputText("(");
			output.outputObject(lock);
			if(acq != null) {
				output.outputText(" by ");
				output.outputObject(acq);
			}
			output.outputText(")");
		}
		
	}
	
	public void postLockAcquired(LockBase lockBase, LockList acq) {
		if(ENABLED) {
			putQueue(new LockEvent("Acquired", lockBase, acq));
		}
	}

	public void postLockEnqueued(LockBase lockBase, LockList acq) {
		if(ENABLED) {
			putQueue(new LockEvent("Enqueued", lockBase, acq));
		}
	}

	public void postLockDequeued(LockBase lockBase, LockList acq) {
		if(ENABLED) {
			putQueue(new LockEvent("Dequeued", lockBase, acq));
		}
	}

	public void postLockFreed(LockBase lockBase) {
		if(ENABLED) {
			putQueue(new LockEvent("Freed", lockBase, null));
		}
	}

	static class NewIntervalEvent extends Event {
		final IntervalImpl inter;
		final IntervalImpl creator;
		
		public NewIntervalEvent(
				IntervalImpl inter,
				IntervalImpl creator) 
		{
			super(inter.end);
			this.inter = inter;
			this.creator = creator;
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("NewInterval");
			output.outputText("(");
			output.outputObject(inter);
			output.outputText(" by ");
			output.outputObject(creator);
			output.outputText(")");
		}
		
	}
	
	public void postNewInterval(IntervalImpl inter, IntervalImpl creator) {
		if(ENABLED) {
			putQueue(new NewIntervalEvent(inter, creator));
		}
	}

	static class FreshWorkerEvent extends Event {
		public final Worker worker;
		public final Medallion medallion;

		public FreshWorkerEvent(Worker worker, Medallion medallion) {
			super(null);
			this.worker = worker;
			this.medallion = medallion;
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText("FreshWorker(");
			output.outputObject(worker);
			output.outputText(" with ");
			output.outputObject(medallion);
			output.outputText(")");
		}
	}
	
	public void postStartedFreshWorker(Worker worker, Medallion medallion) {
		if(ENABLED) {
			putQueue(new FreshWorkerEvent(worker, medallion));
		}
	}
	
	static class TaskEvent extends Event {
		public final String kind;
		public final Worker worker;
		public final Medallion medallion;
		public final Medallion victim;
		public final WorkItem workItem;
		
		public TaskEvent(
				String kind, 
				Worker worker,
				Medallion medallion,
				Medallion victim, 
				WorkItem workItem) {
			super(null);
			this.kind = kind;
			this.worker = worker;
			this.medallion = medallion;
			this.victim = victim;
			this.workItem = workItem;
		}
		
		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText(kind);
			output.outputText("Task(");
			
			if(worker != null) {
				output.outputText("worker=");
				output.outputObject(worker);
				output.outputText(", medallion=");
				output.outputObject(medallion);
				output.outputText(", ");
			}
			
			if(victim != null) {
				output.outputText("victim=");
				output.outputObject(victim);
				output.outputText(", ");
			}
			
			output.outputText("item=");
			output.outputObject(workItem);
			
			output.outputText(")");
		}
	}

	public void postTookTask(
			Worker worker,
			Medallion medallion, 
			WorkItem item) 
	{
		if(ENABLED) {
			putQueue(new TaskEvent("Took", worker, medallion, null, item));
		}
	}
	
	public void postStoleTask(
			Worker thief,
			Medallion medallion, 
			Medallion victim,
			WorkItem item) 
	{
		if(ENABLED) {
			putQueue(new TaskEvent("Stole", thief, medallion, victim, item));
		}
	}

	public void postRemovePendingTask(
			Worker worker,
			Medallion medallion, 
			WorkItem item) 
	{
		if(ENABLED) {
			putQueue(new TaskEvent("Pending", worker, medallion, null, item));
		}
	}

	public void postAddPendingTask(
			WorkItem item) 
	{
		if(ENABLED) {
			putQueue(new TaskEvent("AddPending", null, null, null, item));
		}
	}
	
	static class MedallionEvent extends Event {
		public final String kind;
		public final Medallion medallion;
		public final Worker worker;
		public final Worker awakened;
		
		public MedallionEvent(
				String kind,
				Medallion medallion, 
				Worker worker, 
				Worker awakened) 
		{
			super(null);
			this.kind = kind;
			this.medallion = medallion;
			this.worker = worker;
			this.awakened = awakened;
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			output.outputText(kind);
			output.outputText("Medallion(");
			output.outputObject(medallion);
			output.outputText(", worker=");
			output.outputObject(worker);
			output.outputText(", awakened=");
			output.outputObject(awakened);
			output.outputText(")");
		}
		
	}

	public void postCededMedallion(
			Worker worker, 
			Medallion oldMedallion,
			Worker waiting) 
	{
		if(ENABLED) {
			putQueue(new MedallionEvent("Ceded", oldMedallion, worker, waiting));
		}		
	}

	public void postForkedMedallion(
			Worker worker, 
			Medallion oldMedallion)
	{
		if(ENABLED) {
			putQueue(new MedallionEvent("Forked", oldMedallion, worker, null));
		}		
	}

	public void postReleasedMedallion(
			Worker worker, 
			Medallion oldMedallion,
			Worker awakened)
	{
		if(ENABLED) {
			putQueue(new MedallionEvent("Released", oldMedallion, worker, awakened));
		}		
	}

	public void postReacquiredMedallion(
			Worker worker, 
			Medallion aMedallion) 
	{
		if(ENABLED) {
			putQueue(new MedallionEvent("Reacquired", aMedallion, worker, null));
		}		
	}
	
	public void postWaitingForMedallion(
			Worker worker) 
	{
		if(ENABLED) {
			putQueue(new MedallionEvent("Waiting", null, worker, null));
		}		
	}


	// Lathos routines:
	// ----------------
	
	class EventDumper {
		final Map<RefEvent, DecRefEvent> unmatched = new HashMap<RefEvent, DecRefEvent>();
		
		public void startMainTable(Output out)
		throws IOException
		{
			out.startPage(null);
			out.startBold();
			out.outputText("Event Listing");
			out.endBold();
			out.startTable();
			Lathos.headerRow(out, "id", "event", "comments");
		}
		
		public void dump(Output out, Event event) 
		throws IOException {
			out.startRow();
			
			out.startColumn();
			out.outputText(Long.toString(event.id));
			out.endColumn();

			out.startColumn();
			out.outputObject(event);
			out.endColumn();

			out.startColumn();
			
			event.outputComments(out);
			
//			if(event instanceof DecRefEvent) {
//				DecRefEvent decRef = (DecRefEvent) event;
//				assert decRef.nextUnmatched == null;
//				DecRefEvent oldRef = unmatched.put(decRef, decRef);
//				if(oldRef != null)
//					decRef.nextUnmatched = oldRef;
//			}
//			
//			if(event instanceof AddRefEvent) {
//				AddRefEvent addRef = (AddRefEvent) event;
//				DecRefEvent match = unmatched.get(addRef);
//				if(match == null) {
//					out.startBold();
//					out.outputText("unmatched");
//					out.endBold();
//				} else {
//					if(match.nextUnmatched != null) {
//						unmatched.put(match, match.nextUnmatched);
//						match.nextUnmatched = null;
//					} else {
//						unmatched.remove(match);
//					}
//					out.outputText("matched with event ");
//					out.outputText(Long.toString(match.id));
//				} 
//			}
			
			out.endColumn();
			
			out.endRow();
		}
		
		public void endMainTable(Output out)
		throws IOException
		{
			out.endTable();
			out.endPage(null);
		}
		
		public void dumpUnmatchedTable(Output out)
		throws IOException
		{
//			if(!unmatched.isEmpty()) {
//				out.startPage(null);
//				out.startBold();
//				out.outputText("Unmatched refs");
//				out.endBold();
//				out.startTable();
//				Lathos.headerRow(out, "Unmatched id", "event");
//				for(Map.Entry<?, DecRefEvent> entry : unmatched.entrySet()) {
//					DecRefEvent next = entry.getValue();
//					while(next != null) {
//						DecRefEvent unmatched = next;
//						Lathos.row(out, unmatched.id, unmatched);
//						next = unmatched.nextUnmatched;
//						unmatched.nextUnmatched = null;
//					}
//				}
//				out.endTable();
//				out.endPage(null);
//			}
		}
	}
	
	public synchronized void renderEventsForObject(Output out, Object obj) 
	throws IOException {
		EventList objectList = perObjectLists.get(obj);
		if(objectList == null) {
			out.outputText("No events.");
		} else {
			out.outputText(objectList.size + " total events.");
			EventDumper dump = new EventDumper();
			dump.startMainTable(out);
			for(Event event = objectList.last; event != null; event = event.prevObject) {
				dump.dump(out, event);
			}
			dump.endMainTable(out);
			dump.dumpUnmatchedTable(out);
		}
	}
	
	@Override
	public synchronized void renderInPage(Output out) 
	throws IOException {
		out.startPage(this);
		
		if(!ENABLED) {
			out.startPar();
			out.startBold();
			out.outputText("Note: Disabled!");
			out.endBold();
			out.endPar();
		}
		
		if(allEvents.first != null) {
			out.outputText(allEvents.size + " total events.");
			EventDumper dump = new EventDumper();
			dump.startMainTable(out);
			for(Event event = allEvents.last; event != null; event = event.prevAll) {
				dump.dump(out, event);
			}
			dump.endMainTable(out);
			dump.dumpUnmatchedTable(out);
		} else {
			out.outputText("No events.");
		}
		out.endPage(this);
	}

	@Override
	public void renderInLine(Output output) throws IOException {
		Lathos.renderInLine(this, output);
	}
	
	@Override
	public String toString() {
		return getId();
	}

	@Override
	public String getId() {
		return "IntervalsDebug";
	}

	@Override
	public Page getParent() {
		return null;
	}

	@Override
	public void addContent(PageContent content) {
		throw new UnsupportedOperationException();
	}

}
