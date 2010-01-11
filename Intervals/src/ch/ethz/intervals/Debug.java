package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import ch.ethz.intervals.IndexedInterval.Subtask;
import ch.ethz.intervals.ThreadPool.WorkItem;
import ch.ethz.intervals.ThreadPool.Worker;

class Debug {
	
	public static final boolean ENABLED = false;
	public static final boolean DUMP_IMMEDIATELY = true;
	
	public static final boolean ENABLED_LOCK = true;         /** Debug statements related to locks. */
	public static final boolean ENABLED_WAIT_COUNTS = true;	 /** Debug statements related to wait counts. */
	public static final boolean ENABLED_INTER = false;       /** Debug statements related to higher-level interval control-flow */
	public static final boolean ENABLED_WORK_STEAL = false;  /** Debug statements related to the work-stealing queue */
	
	public static List<Event> events = Collections.synchronizedList(new ArrayList<Event>());
	
	public static void addEvent(Event e) {
		assert ENABLED; // all debug actions should be protected by if(ENABLED)
		if(ENABLED) { // just in case.
			if(DUMP_IMMEDIATELY) {
				synchronized(events) {
					System.err.println(e.toString());
				}
			} else
				events.add(e);
		}
	}
	
	public static void dump() {
		synchronized(events) {
			for(Event e : events)
				System.err.println(e.toString());
			events.clear();
		}
	}

	static abstract class Event {}

	static class ArriveEvent extends Event {
		public final Point point;
		public final int count;
		public final int newCount;
		
		public ArriveEvent(Point point, int count, int newCount) {
			this.point = point;
			this.count = count;
			this.newCount = newCount;
		}				
		
		public String toString() {
			return String.format("ARRIVE %s count=%d newCount=%d", point, count, newCount);
		}
	}
	
	public static void arrive(Point point, int count, int newCount) {
		if(ENABLED_WAIT_COUNTS)
			addEvent(new ArriveEvent(point, count, newCount));
	}

	static class OccurEvent extends Event {
		public final Point point;
		public final EdgeList list;
		
		public OccurEvent(Point point, EdgeList list) {
			this.point = point;
			this.list = list;
		}				
		
		public String toString() {
			final StringBuilder sb = new StringBuilder();
			sb.append(String.format("OCCUR %s nextEpoch %s bound %s succs", point, point.nextEpoch, point.line.bound));
			
			new EdgeList.Iterator(list) {
				public void doForEach(Point toPoint, int flags) {
					if(EdgeList.waiting(flags))
						sb.append(String.format(" %s(%x)", toPoint, flags & EdgeList.ALL_FLAGS));					
				}
			};
			
			return sb.toString();
		}
	}
	
	public static void occur(Point point, EdgeList list) {
		if(ENABLED_WAIT_COUNTS)
			addEvent(new OccurEvent(point, list));
	}
	
	static class JoinEvent extends Event {
		public final Point joinedPnt;
		
		public JoinEvent(Point joinedPnt) {
			this.joinedPnt = joinedPnt;
		}
		
		public String toString() {
			return String.format("JOIN %s", joinedPnt);
		}
	}
	
	public static void join(Point joinedPnt) {
		addEvent(new JoinEvent(joinedPnt));
	}

	static class SubIntervalEvent extends Event {
		public final Interval inter;
		public final String description;

		public SubIntervalEvent(Interval inter, String description) {
			this.inter = inter;
			this.description = description;
		}
		
		public String toString() {
			return String.format("SUB %s-%s end.nextEpoch=%s desc=%s", inter.start, inter.end, inter.end.nextEpoch, description);
		}
	}
	
	public static void subInterval(Interval inter, String description) {
		if(ENABLED_INTER)
			addEvent(new SubIntervalEvent(inter, description));
	}

	static class NewIntervalEvent extends Event {
		public final Interval inter;
		public final String description;

		public NewIntervalEvent(Interval inter, String description) {
			this.inter = inter;
			this.description = description;
		}
		
		public String toString() {
			return String.format("NEW %s desc=%s", inter, description);
		}
	}
	
	public static void newInterval(Interval inter, String description) {
		if(ENABLED_INTER)
			addEvent(new NewIntervalEvent(inter, description));
	}

	static class AddWaitCountEvent extends Event {
		public final Point point;		
		public final int newCount;
		
		public AddWaitCountEvent(Point point, int newCount) {
			this.point = point;
			this.newCount = newCount;
		}
		
		public String toString() {
			return String.format("ADD_WAIT_COUNT %s newCount=%d", point, newCount);
		}
	}
	
	public static void addWaitCount(Point point, int newCount) {
		if(ENABLED_WAIT_COUNTS)
			addEvent(new AddWaitCountEvent(point, newCount));
	}
	
	static class AwakenIdleEvent extends Event {
		public final Worker awakenedByWorker;
		public final WorkItem workItem;
		public final Worker idleWorker;
		
		public AwakenIdleEvent(Worker awakenWorker, WorkItem workItem, Worker idleWorker) {
			this.awakenedByWorker = awakenWorker;
			this.workItem = workItem;
			this.idleWorker = idleWorker;
		}
		
		public String toString() {
			return String.format("AWAKEN_IDLE %s workItem=%s awakenedBy=%s", idleWorker, workItem, awakenedByWorker);
		}
	}
	
	public static void awakenIdle(Worker worker, WorkItem workItem,
			Worker idleWorker) {
		if(ENABLED_WORK_STEAL)
			addEvent(new AwakenIdleEvent(worker, workItem, idleWorker));
	}
	
	static class EnqueueEvent extends Event {
		public final Worker enqueueWorker;
		public final WorkItem item;
		
		public EnqueueEvent(Worker enqueueWorker, WorkItem item) {
			this.enqueueWorker = enqueueWorker;
			this.item = item;
		}
		
		public String toString() {
			return String.format("ENQUEUE %s item=%s", enqueueWorker, item);
		}
	}
	
	public static void enqeue(Worker worker, WorkItem item) {
		if(ENABLED_WORK_STEAL)
			addEvent(new EnqueueEvent(worker, item));
	}
	
	static class QueueOpEvent extends Event {
		public final String kind;
		public final Worker victim; // or owner
		public final Worker thief;
		public final int h1, s1, g1;
		public final WorkItem task;
		public final int h2, s2, g2;
		
		public QueueOpEvent(String kind, Worker victim, Worker thief, int h1,
				int s1, int g1, WorkItem task, int h2, int s2, int g2) {
			super();
			this.kind = kind;
			this.victim = victim;
			this.thief = thief;
			this.h1 = h1;
			this.s1 = s1;
			this.g1 = g1;
			this.task = task;
			this.h2 = h2;
			this.s2 = s2;
			this.g2 = g2;
		}
		
		public String toString() {
			return String.format("QUEUE_%s %s thief=%s before=<%d,%d,%d> after=<%d,%d,%d> item=%s",
					kind, victim, thief, h1, s1, g1, h2, s2, g2, task);
		}
	}

	public static void queueOp(String kind, Worker victim, Worker thief,
			int h1, int s1, int g1, WorkItem task, int h2, int s2, int g2) {
		if(ENABLED_WORK_STEAL)
			addEvent(new QueueOpEvent(kind, victim, thief, h1, s1, g1, task, h2, s2, g2));
	}

	static class DequePutEvent extends Event {
		public final Worker owner;
		public final int l, ownerHead, ownerTail, taskIndex;
		public final WorkItem task;

		public DequePutEvent(Worker owner, int l, int ownerHead, int ownerTail,
				int taskIndex, WorkItem task) {
			super();
			this.owner = owner;
			this.l = l;
			this.ownerHead = ownerHead;
			this.ownerTail = ownerTail;
			this.task = task;
			this.taskIndex = taskIndex;
		}



		public String toString() {
			return String.format("DEQUE_PUT %s l=%d owner=%d-%d tasks[%d]=%s", 
					owner, l, ownerHead, ownerTail, taskIndex, task);
		}
	}

	public static void dequePut(Worker owner, int l, int ownerHead, int ownerTail,
			int taskIndex, WorkItem task) {
		if(ENABLED_WORK_STEAL)
			addEvent(new DequePutEvent(owner, l, ownerHead, ownerTail, taskIndex, task));
	}

	static class DequeTakeEvent extends Event {
		public final Worker owner;
		public final int l, ownerHead, ownerTail, lastIndex;
		public final WorkItem task;

		public DequeTakeEvent(Worker owner, int l, int ownerHead, int ownerTail,
				int lastIndex, WorkItem task) {
			super();
			this.owner = owner;
			this.l = l;
			this.ownerHead = ownerHead;
			this.ownerTail = ownerTail;
			this.lastIndex = lastIndex;
			this.task = task;
		}

		public String toString() {
			return String.format("DEQUE_TAKE %s l=%d owner=%d-%d tasks[%d]=%s",
					owner, l, ownerHead, ownerTail, lastIndex, task);
		}
	}

	public static void dequeTake(Worker owner, int l, int ownerHead, int ownerTail,
			int lastIndex, WorkItem task) {
		if(ENABLED_WORK_STEAL)
			addEvent(new DequeTakeEvent(owner, l, ownerHead, ownerTail, lastIndex, task));
	}
	
	static class DequeStealEvent extends Event {
		public final Worker victimWorker, thiefWorker;
		public final int thiefHead;
		public final int taskIndex;
		public final WorkItem task;

		public DequeStealEvent(Worker victimWorker, Worker thiefWorker,
				int thiefHead, int taskIndex, WorkItem task) {
			super();
			this.victimWorker = victimWorker;
			this.thiefWorker = thiefWorker;
			this.thiefHead = thiefHead;
			this.taskIndex = taskIndex;
			this.task = task;
		}

		public String toString() {
			return String.format("DEQUE_STEAL %s thief=%s head=%d tasks[%d]=%s",
					victimWorker, thiefWorker, thiefHead, taskIndex, task);
		}
	}
	
	public static void dequeSteal(Worker victimWorker, Worker thiefWorker, int thiefHead, int taskIndex, WorkItem task) {
		if(ENABLED_WORK_STEAL)
			addEvent(new DequeStealEvent(victimWorker, thiefWorker, thiefHead, taskIndex, task));
	}
	
	static class MapForkEvent extends Event {
		public final Subtask mapBase, mapFork;
		public final int b;
		public final IndexedInterval mapTask;
		
		public MapForkEvent(IndexedInterval mapTask, Subtask mapBase, Subtask mapFork, int b) {
			this.mapTask = mapTask;
			this.mapBase = mapBase;
			this.mapFork = mapFork;
			this.b = b;
		}
		
		public String toString() {
			return String.format("MAP_FORK %s base=%s new=%s balance=%d", 
					mapTask, mapBase, mapFork, b);
		}
	}

	public static void mapFork(IndexedInterval mapTask, Subtask mapBase, Subtask mapFork, int b) {
		if(ENABLED_WORK_STEAL)
			addEvent(new MapForkEvent(mapTask, mapBase, mapFork, b));
	}

	static class MapCompleteEvent extends Event {
		public final IndexedInterval mapTask;
		public final Subtask mapBase;
		public final int b;
		public final Point whenDone;
		
		public MapCompleteEvent(IndexedInterval mapTask, Subtask mapBase, int b, Point whenDone) {
			this.mapTask = mapTask;
			this.mapBase = mapBase;
			this.b = b;
			this.whenDone = whenDone;
		}
		
		public String toString() {
			return String.format("MAP_COMPLETE %s base=%s balance=%d whenDone=%s", 
					mapTask, mapBase, b, whenDone);
		}
	}

	public static void mapComplete(IndexedInterval mapTask, Subtask mapBase, int b, Point whenDone) {
		if(ENABLED_WORK_STEAL)
			addEvent(new MapCompleteEvent(mapTask, mapBase, b, whenDone));
	}

	static class MapRunEvent extends Event {
		public final IndexedInterval mapTask;
		public final Subtask mapBase;
		public final int l, h;
		
		public MapRunEvent(IndexedInterval mapTask, Subtask mapBase, int l, int h) {
			this.mapTask = mapTask;
			this.mapBase = mapBase;
			this.l = l;
			this.h = h;
		}
		
		public String toString() {
			return String.format("MAP_RUN %s base=%s range=%d-%d",
					mapTask, mapBase, l, h);
		}
	}
	
	public static void mapRun(IndexedInterval mapTask, Subtask mapBase, int l, int h) {
		if(ENABLED_WORK_STEAL)
			addEvent(new MapRunEvent(mapTask, mapBase, l, h));
	}
	
	static class ExecuteEvent extends Event {
		public final Worker worker;
		public final WorkItem item;
		public final boolean started;
		
		public ExecuteEvent(Worker worker, WorkItem item, boolean started) {
			this.worker = worker;
			this.item = item;
			this.started = started;
		}
		
		public String toString() {
			return String.format("EXECUTE %s started=%s item=%s", worker, started, item);
		}
	}
	
	public static void execute(Worker worker, WorkItem item, boolean started) {
		if(ENABLED_WAIT_COUNTS)
			addEvent(new ExecuteEvent(worker, item, started));
	}
	
	static class AcquireLockEvent extends Event {
		public final LockBase lockBase;
		public final LockList acq;
		
		public AcquireLockEvent(LockBase lockBase, LockList acq) {
			this.lockBase = lockBase;
			this.acq = acq;
		}

		public String toString() {
			return String.format("ACQUIRE_LOCK %s lockList=%s", lockBase, acq);
		}
	}

	public static void acquireLock(LockBase lockBase, LockList acq) {
		if(ENABLED_LOCK)
			addEvent(new AcquireLockEvent(lockBase, acq));
	}

	static class QueueForLockEvent extends Event {
		public final LockBase lockBase;
		public final LockList acq;
		
		public QueueForLockEvent(LockBase lockBase, LockList acq) {
			this.lockBase = lockBase;
			this.acq = acq;
		}

		public String toString() {
			return String.format("ENQUEUE_FOR_LOCK %s start=%s", lockBase, acq);
		}
	}

	public static void enqueueForLock(LockBase lockBase, LockList acq) {
		if(ENABLED_LOCK)
			addEvent(new QueueForLockEvent(lockBase, acq));
	}

	static class DequeueForLockEvent extends Event {
		public final LockBase lockBase;
		public final LockList acq;
		
		public DequeueForLockEvent(LockBase lockBase, LockList acq) {
			this.lockBase = lockBase;
			this.acq = acq;
		}

		public String toString() {
			return String.format("DEQUEUE_FOR_LOCK %s start=%s", lockBase, acq);
		}
	}

	public static void dequeueForLock(LockBase lockBase, LockList acq) {
		if(ENABLED_LOCK)
			addEvent(new DequeueForLockEvent(lockBase, acq));
	}
	
	static class LockFreeEvent extends Event {
		public final LockBase lockBase;
		
		public LockFreeEvent(LockBase lockBase) {
			this.lockBase = lockBase;
		}

		public String toString() {
			return String.format("LOCK_FREE lock=%s", lockBase);
		}
	}

	public static void lockFree(LockBase lockBase) {
		if(ENABLED_LOCK)
			addEvent(new LockFreeEvent(lockBase));
	}
	
}
