package ch.ethz.intervals.impl;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import pcollections.PSet;
import ch.ethz.intervals.impl.IntervalImpl.State;

import com.smallcultfollowing.lathos.CustomOutput;
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
	 * Enable debugging.  Sometimes easier to edit source file than to 
	 * adjust the properties! */
	private static final boolean STATIC_SWITCH = false; 

	/**
	 * Controls whether debugging is enabled.   
	 * Initialized from the property {@code IntervalsDebug}.
	 * Defaults to {@code false}. */
	public static final boolean ENABLED = STATIC_SWITCH || Boolean.parseBoolean(System.getProperty("IntervalsDebug", "false"));
	
	/** 
	 * Throw out events if we have more than this amount, unless the relevant
	 * point has not yet occurred. 
	 * 
	 * Initialized from the property {@code IntervalsDebugMemory1}.
	 * Defaults to {@code 10000}. */
	public static final long MEMORY1 = Long.parseLong(System.getProperty("IntervalsDebugMemory1", "10000"));
	
	/** 
	 * Unconditionally remove events when we have accumulated this amount. 
	 * 
	 * Initialized from the property {@code IntervalsDebugMemory2}.
	 * Defaults to {@code 100000}. */
	public static final long MEMORY2 = Long.parseLong(System.getProperty("IntervalsDebugMemory2", "100000"));
	
	/**
	 * Singleton instance. */
	public static final Debug debug = new Debug();
	
	private Debug() {
		updateThread = new UpdateThread();
		updateThread.start();
	}
	
	// Interval data structures:
	// -------------------------
	//
	// Accessed only while holding lock on the RefTracker.
	// Generally this is done during the update thread.
	
	private final Map<PointImpl, EventList> perObjectLists = new HashMap<PointImpl, EventList>(); 
//	private final Map<RefEvent, AddRefEvent> unmatchedAddRefEvents = new HashMap<RefEvent, AddRefEvent>();
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

	// Update thread:
	// --------------

	private class EventList {
		public long size;
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
	}
	
	private BlockingQueue<Event> queue = new LinkedBlockingQueue<Debug.Event>();
	
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
	
	// Posting routines:
	// -----------------
	
	abstract static class RefEvent extends Event {
		final Object from;
		
		public RefEvent(PointImpl point, Object from) {
			super(point);
			this.from = from;
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
		// add ref is not yet matched against a DecRef.  If it points
		// to a DecRefEvent, then it has been matched.
//		RefEvent nextUnmatched;
		
		public AddRefEvent(PointImpl point, Object from, int newValue) {
			super(point, from);
			this.newValue = newValue;
		}
		
//		@Override
//		public void post(Debug debug) {
//			super.post(debug);
//			
//			nextUnmatched = debug.unmatchedAddRefEvents.put(this, this);
//		}
//		
//		private boolean isMatched() {
//			return (nextUnmatched != null) && (nextUnmatched instanceof DecRefEvent);
//		}
//
//		@Override
//		public boolean irrelevant() {
//			return super.irrelevant() || isMatched();
//		}

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
			queue.add(new AddRefEvent(point, from, newValue));
		}
	}

	static class DecRefEvent extends RefEvent {
		final int newValue;
		DecRefEvent nextUnmatched; // normally null except when printing out
		
		public DecRefEvent(PointImpl point, Object from, int newValue) {
			super(point, from);
			this.newValue = newValue;
		}
		
//		@Override
//		public void post(Debug debug) {
//			super.post(debug);
//			
//			AddRefEvent match = debug.unmatchedAddRefEvents.get(this);
//			if(match == null) {
//				// unmatched add. bad.
//			} else {
//				if(match.nextUnmatched == null)
//					debug.unmatchedAddRefEvents.remove(match);
//				else
//					debug.unmatchedAddRefEvents.put(
//							match.nextUnmatched, 
//							(AddRefEvent) match.nextUnmatched);
//				
//				match.nextUnmatched = this;
//			}
//		}

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
			queue.add(new DecRefEvent(point, from, newValue));
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
			queue.add(new OccurEvent(point));
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
			queue.add(new ScheduleEvent(scheduled, scheduledBy));
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
			queue.add(new TransitionEvent(intervalImpl, state, newState));
		}
	}
	
	class VertExceptionEvent extends Event {
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
			queue.add(new VertExceptionEvent(intervalImpl, vertExceptions.size()));
		}
	}

	class LockEvent extends Event {
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
			queue.add(new LockEvent("Acquired", lockBase, acq));
		}
	}

	public void postLockEnqueued(LockBase lockBase, LockList acq) {
		if(ENABLED) {
			queue.add(new LockEvent("Enqueued", lockBase, acq));
		}
	}

	public void postLockDequeued(LockBase lockBase, LockList acq) {
		if(ENABLED) {
			queue.add(new LockEvent("Dequeued", lockBase, acq));
		}
	}

	public void postLockFreed(LockBase lockBase) {
		if(ENABLED) {
			queue.add(new LockEvent("Freed", lockBase, null));
		}
	}

	class NewIntervalEvent extends Event {
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
			queue.add(new NewIntervalEvent(inter, creator));
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
			
			if(event instanceof DecRefEvent) {
				DecRefEvent decRef = (DecRefEvent) event;
				assert decRef.nextUnmatched == null;
				DecRefEvent oldRef = unmatched.put(decRef, decRef);
				if(oldRef != null)
					decRef.nextUnmatched = oldRef;
			}
			
			if(event instanceof AddRefEvent) {
				AddRefEvent addRef = (AddRefEvent) event;
				DecRefEvent match = unmatched.get(addRef);
				if(match == null) {
					out.startBold();
					out.outputText("unmatched");
					out.endBold();
				} else {
					if(match.nextUnmatched != null) {
						unmatched.put(match, match.nextUnmatched);
						match.nextUnmatched = null;
					} else {
						unmatched.remove(match);
					}
					out.outputText("matched with event ");
					out.outputText(Long.toString(match.id));
				} 
			}
			
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
			if(!unmatched.isEmpty()) {
				out.startPage(null);
				out.startBold();
				out.outputText("Unmatched refs");
				out.endBold();
				out.startTable();
				Lathos.headerRow(out, "Unmatched id", "event");
				for(Map.Entry<?, DecRefEvent> entry : unmatched.entrySet()) {
					DecRefEvent next = entry.getValue();
					while(next != null) {
						DecRefEvent unmatched = next;
						Lathos.row(out, unmatched.id, unmatched);
						next = unmatched.nextUnmatched;
						unmatched.nextUnmatched = null;
					}
				}
				out.endTable();
				out.endPage(null);
			}
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
