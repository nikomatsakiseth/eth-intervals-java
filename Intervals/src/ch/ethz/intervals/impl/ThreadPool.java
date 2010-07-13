package ch.ethz.intervals.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReferenceArray;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;
import com.smallcultfollowing.lathos.PageContent;

/**
 * Intervals Thread Pool.  A work stealing thread pool
 * customized to the needs of intervals.
 */
public class ThreadPool {
	
	/* The design allows for a fixed amount of parallelism.
	 * Most of the time this just means you have a fixed
	 * pool of worker threads pulling tasks off of various
	 * queues and executing them.
	 * 
	 * Although there are only a fixed number of threads
	 * doing work at any one time, there may be any number
	 * of worker threads.  This is because threads that
	 * are forced to block (such as when waiting for an 
	 * inline interval) must spin off new threads to keep
	 * the system from stalling.
	 * 
	 * The thread pool is created with a fixed number of
	 * Medallions.  A worker thread must hold the medallion
	 * to do work, and when it needs to block it yields the
	 * medallion to other threads.  When it wants to resume,
	 * requests a new medallion (which might not be the
	 * same one it had before).
	 * 
	 * Each medallion has an associated deque.  The thread
	 * holding the medallion will pop tasks off the deque;
	 * if there are no tasks, it will then try to steal
	 * from other medallions.  If that fails, it will 
	 * eventually fall back to a global queue or simply
	 * block until work arrives.
	 * 
	 * When a thread completes a task, however, it also
	 * checks to see if any previously suspended workers
	 * have requested a medallion.  If so, it will exit,
	 * as we consider it a priority for the suspended task
	 * to resume and terminate.
	 */
	
/* We used to use a KeepAliveThread to prevent the VM from
 * terminating while intervals are active.  In the new API,
 * however, the user must always use a blocking inline
 * interval as the "root" interval, and thus we don't need
 * to worry about that anymore, so long as the root interval
 * was itself started from a non-daemon thread.  Horray.
 * 
 * I left the code here, however, in case this issue should
 * come back to haunt me.  The idea was to start 
 * the KeepAliveThread whenever we gave out our first medallion
 * and stop it whenever we all medallions became free again.
 */
	
//	class KeepAliveThread extends Thread {		
//		public final Semaphore sem = new Semaphore(1);
//		
//		@Override
//		public void run() {
//			sem.acquireUninterruptibly();
//			sem.release();
//			return;
//		}		
//	}
//	
//	private KeepAliveThread keepAliveThread;
//	
//	/** 
//	 * Starts a keep alive thread that prevents the JVM from
//	 * exiting. This is invoked when new work is submitted to
//	 * the pool from the outside. Always executed under the {@link #idleLock}.
//	 */
//	private void startKeepAliveThread() {
//		if(keepAliveThread == null) {
//			keepAliveThread = new KeepAliveThread();
//			keepAliveThread.sem.acquireUninterruptibly();
//			keepAliveThread.start();
//		}		
//	}
//	
//	/** 
//	 * Stops the keep alive thread that prevents the JVM from
//	 * exiting.  This is invoked when all threads become
//	 * idle.  Always executed under the {@link #idleLock}.
//	 */
//	private void stopKeepAliveThread() {
//		if(keepAliveThread != null) {
//			keepAliveThread.sem.release();
//			keepAliveThread = null;
//		}
//	}
	
	static class LazyDeque {
		static class ThiefData {
			int head = 0;
		}
		
		static final int INITIAL_SIZE = (1 << 10);
		static final int PAD = 0, OFFSET = 0; 
		
		private AtomicReferenceArray<WorkItem> tasksArray = new AtomicReferenceArray<WorkItem>(size(INITIAL_SIZE));
		
		int ownerHead = 0, ownerTail = 0;
		private final ThiefData thief = new ThiefData();
		
		private int index(int id) {
			return index(tasksArray.length() >> PAD, id);
		}
		
		private static int size(int l) {
			return (l << PAD) + OFFSET;
		}
		
		private static int index(int l, int id) {
			return ((id % l) << PAD) + OFFSET;
		}
		
		public void put(Medallion owner, WorkItem task) { // Only owner can put.
			assert task != null;
			while(true) {
				final int l = tasksArray.length() >> PAD;
				final int tail = ownerTail;
				
				if(tail - ownerHead >= l || tail == Integer.MAX_VALUE) {
					expand(); // Would be full or would roll-over
					continue;
				}
				
				final int index = index(l, tail);
				tasksArray.set(index, task);
				ownerTail = tail + 1; // important: put vol. wr. after array store to create HB rel
				
//				if(Debug.ENABLED)
//					Debug.dequePut(owner, l, ownerHead, ownerTail, tail, task);				
				return;
			}
		}
		
		public WorkItem take(Medallion owner) { 
			// Only owner can take.  Returns either NULL or a WorkItem that should be executed.
			if(ownerHead == ownerTail)
				return null; // Empty.
			
			// Pop the last item from the deque.
			final int lastTail = ownerTail - 1;
			final int lastIndex = index(lastTail);
			
			// Read the item popped.
			//   Note: if we get back null, the item must have been stolen, since otherwise
			//   we never store null into the array, and we know this location was initialized.
			WorkItem item = tasksArray.getAndSet(lastIndex, null);
			
			try {
				if(item == null) { // The item we put here was stolen!
					// If this item was stolen, then all previous entries
					// must have been stolen too.  Update our notion of the head of
					// the deque.
					ownerHead = ownerTail;
					return null; // Deque is now empty.						
				}
				
				ownerTail = lastTail;
				return item;
			} finally { 
//				if(Debug.ENABLED)
//					Debug.dequeTake(owner, tasksArray.length() >> PAD, ownerHead, ownerTail, lastIndex, item);				
			}				
		}
		
		public WorkItem steal(Medallion victimWorker, Medallion thiefWorker) {
			synchronized(thief) { // At most one thief at a time.
				final int head = thief.head;
				final int index = index(head);
				WorkItem item = tasksArray.getAndSet(index, null);
				
//				if(Debug.ENABLED)
//					Debug.dequeSteal(victimWorker, thiefWorker, thief.head, index, item);
				
				if(item == null) // if null, was either already taken by owner or never there.
					return null; 
				
				thief.head++; // Successfully stolen!
				return item;
			}			
		}
		
		public void expand() { // Only owner can expand.
			synchronized(thief) { // No thieves are active.
				assert ownerHead <= thief.head && thief.head <= ownerTail;
				ownerHead = thief.head;
				
				int l = tasksArray.length() >> PAD, thold = l >> 4;
				int size = (ownerTail - ownerHead);
				if((l - size) <= thold) { // Less than 1/16 is free.
					replaceTaskArray(l * 2);
				} else if (ownerTail == Integer.MAX_VALUE) { // About to roll-over.
					replaceTaskArray(l); 
				}
			}
		}

		private void replaceTaskArray(int size) {
			AtomicReferenceArray<WorkItem> newTasks = new AtomicReferenceArray<WorkItem>(size(size));
			final int l = tasksArray.length() >> PAD;
			int j = 0;
			for(int i = ownerHead; i < ownerTail; i++)
				newTasks.set(index(size, j++), tasksArray.get(index(l, i)));
			ownerTail = j;
			ownerHead = thief.head = 0;			
			tasksArray = newTasks;
		}

		boolean assertEmpty() {
			boolean result;
			synchronized(thief) { // No thieves are active.
				result = thief.head == ownerTail;
			}			
			return result;
		}
	}
	
	static abstract class WorkItem {
		abstract void exec(Medallion medallion);
	}
	
	final class Worker
	implements Runnable, Page
	{
		/**
		 * Semaphore used for blocking and unblocking this
		 * worker.  It begins with zero permits.  When a 
		 * worker blocks waiting for a medallion, it performs
		 * an acquire.  When a medallion is available, the
		 * semaphore will be released. */
		private final Semaphore semaphore = new Semaphore(0);
		
		/** 
		 * Current medallion assigned to this worker.
		 * Modified by {@code ThreadPool.this}. */
		private Medallion medallion;
		
		/** 
		 * Used to store next worker in the linked list 
		 * when a worker is waiting for a medallion.
		 * Owned by {@code ThreadPool.this}. */
		private Worker nextWaiting;
		
		/**
		 * Only used when debugging is enabled: stores the currently
		 * executing item. */
		private volatile WorkItem debugItem;
		
		Worker(Medallion initial) {
			medallion = initial;
		}
		
		public String toString() {
			return String.format("Worker[%x]", System.identityHashCode(this));
		}
		
		@Override 
		public void run() {
			currentWorker.set(this);
			
			loop: while(true) {
				WorkItem item;
				
				if(waitingWorker != null) {
					if(cedeMedallion())
						break loop;
				}
				
				if((item = takeTask()) != null) {
					exec(item); continue loop;
				}
				
				if((item = stealTask()) != null) {
					exec(item); continue loop;
				}
				
				if((item = findPendingWork()) != null) {
					exec(item); continue loop;
				}
				
				break loop;
			}

			assert medallion == null;
			currentWorker.remove();
		}

		private void exec(WorkItem item) {
			if(Debug.ENABLED) {
				debugItem = item;
			}
			
			item.exec(medallion);
			
			if(Debug.ENABLED) {
				debugItem = null;
			}
		}

		/**
		 * Invoked when we are removed from the waiting worker list. */
		private void deliver(Medallion aMedallion) {
			assert medallion == null;
			nextWaiting = null;
			medallion = aMedallion;
			semaphore.release();
		}

		/**
		 * Invoked when we saw that another worker might
		 * be waiting.  Only cedes the medallion if we
		 * do in fact find such a worker. 
		 * @return true if the medallion was ceded */
		private boolean cedeMedallion() {
			Worker waiting;
			
			synchronized(ThreadPool.this) {
				waiting = waitingWorker;
				if(waiting == null)
					return false;
				waitingWorker = waiting.nextWaiting;
			}
			
			Medallion oldMedallion = medallion;
			medallion = null;

			waiting.deliver(oldMedallion);
			
			if(Debug.ENABLED)
				Debug.debug.postCededMedallion(this, oldMedallion, waiting);
			
			return true;
		}

		/**
		 * Invoked when one of our tasks needs to block. Releases our medallion,
		 * either to another who is waiting (if any) or to a new task (if none). */
		public void yieldMedallion() {
			if(!cedeMedallion()) {
				Medallion oldMedallion = medallion;
				medallion = null;

				startWorkerWith(null, oldMedallion);
				
				if(Debug.ENABLED)
					Debug.debug.postForkedMedallion(this, oldMedallion);
			}
		}
		
		/**
		 * Invoked when we have no more work to do.  Unconditionally
		 * releases our medallion, possibly waking up another worker. */
		private void releaseMedallion() {
			Medallion oldMedallion = medallion;
			medallion = null;
			
			assert oldMedallion.tasks.assertEmpty();
			
			Worker awaken;
			synchronized(ThreadPool.this) {
				awaken = waitingWorker;
				if(awaken == null) {
					oldMedallion.nextFree = freeMedallion;
					freeMedallion = oldMedallion;
				} else {
					waitingWorker = awaken.nextWaiting;
				}
			}
			
			if(awaken != null) {
				awaken.deliver(oldMedallion);
			}
			
			if(Debug.ENABLED)
				Debug.debug.postReleasedMedallion(this, oldMedallion, awaken);
		}

		/**
		 * Invoked when we have finished blocking.
		 * Takes a free medallion, if any are available,
		 * or waits until one becomes available. */
		public void reacquireMedallion() {
			assert medallion == null;
			assert nextWaiting == null;
			
			Medallion aMedallion;
			synchronized(ThreadPool.this) {
				aMedallion = freeMedallion;
				if(aMedallion != null) {
					freeMedallion = aMedallion.nextFree;
				} else {
					nextWaiting = waitingWorker;
					waitingWorker = this;
				}
			}

			if(aMedallion != null) {
				if(Debug.ENABLED)
					Debug.debug.postReacquiredMedallion(this, aMedallion);
				
				aMedallion.nextFree = null;
				medallion = aMedallion;
			} else {
				if(Debug.ENABLED)
					Debug.debug.postWaitingForMedallion(this);
				
				// Block until someone wakes us up.
				// They will have assigned us a medallion.
				semaphore.acquireUninterruptibly();
				assert medallion != null;
			}
		}
		
		/** 
		 * Pops a task for the medallion's deque, if any is available. */
		private WorkItem takeTask() {
			WorkItem item = medallion.takeTask();
			
			if(Debug.ENABLED && item != null)
				Debug.debug.postTookTask(this, medallion, item);
			
			return item;
		}

		/** 
		 * Steals a task from another medallion, if any is available. */
		private WorkItem stealTask() {
			WorkItem item;
			for(int victim = medallion.id + 1; victim < numWorkers; victim++)
				if((item = stealTaskFrom(victim)) != null)
					return item;
			for(int victim = 0; victim < medallion.id; victim++)
				if((item = stealTaskFrom(victim)) != null)
					return item;
			return null;
		}
		
		private WorkItem stealTaskFrom(int victimId) {
			Medallion victim = medallions[victimId];
			WorkItem item = victim.tasks.steal(victim, medallion);
			
			if(Debug.ENABLED && item != null)
				Debug.debug.postStoleTask(this, medallion, victim, item);

			return item;
		}
		
		/** 
		 * Either removes a task from the pendingWorkItems list or 
		 * releases our medallion since no work can be found. */
		private WorkItem findPendingWork() {
			synchronized(ThreadPool.this) {
				int l = pendingWorkItems.size();
				if(l != 0) {
					WorkItem item = pendingWorkItems.remove(l - 1);
					
					if(Debug.ENABLED)
						Debug.debug.postRemovePendingTask(this, medallion, item);
					
					return item;
				} 
				
				releaseMedallion();
				return null;
			}
		}

		@Override
		public void renderInPage(Output out) throws IOException {
			out.startPage(this);
			
			out.startBold();
			out.outputText(toString());
			out.endBold();
			
			out.startTable();
			Lathos.row(out, "Item", debugItem);
			Lathos.row(out, "Medallion", medallion);
			out.endTable();
			
			out.endPage(this);
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			Lathos.renderInLine(this, output);
		}

		@Override
		public String getId() {
			return toString();
		}

		@Override
		public Page getParent() {
			return Debug.debug;
		}

		@Override
		public void addContent(PageContent content) {
			throw new UnsupportedOperationException();
		}

	}
	
	/**
	 * Medallion represents the "right to do intervals
	 * work."  There are only a fixed number, usually
	 * the number of processors on the system. */
	final class Medallion 
	{
		final int id;
		LazyDeque tasks = new LazyDeque();
		Medallion nextFree;
		
		Medallion(int id) {
			this.id = id;
		}
		
		public WorkItem takeTask() {
			return tasks.take(this);
		}

		@Override
		public String toString(){
			return "(Medallion-"+id+")";
		}

		void enqueue(WorkItem item) {
			if(freeMedallion == null || !tryToStartWorkerWith(item)) {
				tasks.put(this, item);
			}
		}
		
	}
	
	/**
	 * Used to spin up new workers when more parallelism is needed. */
	final Executor executor = Executors.newCachedThreadPool(new ThreadFactory() {
		final AtomicInteger counter = new AtomicInteger(0);
		
		@Override
		public Thread newThread(Runnable r) {
			Thread t = new Thread(r, "Intervals-Worker-" + counter.getAndIncrement());
			t.setDaemon(true);
			return t;
		}
	});
	
	/**
	 * Number of medallions issued in total. */
	public final int numWorkers = Runtime.getRuntime().availableProcessors();
	
	/**
	 * Array of all medallions.  Never modified after the constructor. */
	final Medallion[] medallions = new Medallion[numWorkers];
	
	/**
	 * Current medallion assigned to a thread, if any. */
	final static ThreadLocal<Worker> currentWorker = new ThreadLocal<Worker>();
	
	/**
	 * List of work items that were submitted from the outside 
	 * when there were no available medallions.  
	 * Rarely used and should probably be removed. 
	 * 
	 * Accessed only while holding lock. */
	final ArrayList<WorkItem> pendingWorkItems = new ArrayList<WorkItem>();
	
	/**
	 * Head of the linked list of free medallions.  If
	 * null, then there are no available medallions.
	 * 
	 * Modified only while holding lock, but 
	 * read without lock. */
	volatile Medallion freeMedallion;

	/**
	 * Head of the linked list of workers waiting for a 
	 * medallion.  If null, then there are no waiting workers.
	 * 
	 * Modified only while holding lock, but 
	 * read without lock. */
	volatile Worker waitingWorker; 
	
	public ThreadPool() {
		freeMedallion = medallions[0] = new Medallion(0);
		for(int i = 1; i < numWorkers; i++) {
			medallions[i] = new Medallion(i);
			medallions[i-1].nextFree = medallions[i];
		}
	}

	/**
	 * Starts a new worker processing {@code item} if a 
	 * free medallion can be found.
	 * 
	 * @return true if a new worker was started */
	private boolean tryToStartWorkerWith(WorkItem item) {
		Medallion medallion = null;
		
		synchronized(ThreadPool.this) {
			medallion = freeMedallion;
			if(medallion != null) {
				freeMedallion = medallion.nextFree;
			}
		}
			
		if(medallion == null)
			return false;

		startWorkerWith(item, medallion);
		return true;
	}

	/**
	 * Starts a new worker with the given medallion and an
	 * (optional) initial task {@code item}. */
	private void startWorkerWith(WorkItem item, Medallion medallion) {
		medallion.nextFree = null;
		if(item != null)
			medallion.tasks.put(medallion, item);
		Worker worker = new Worker(medallion);
		executor.execute(worker);
		
		if(Debug.ENABLED)
			Debug.debug.postStartedFreshWorker(worker, medallion);
	}
	
	/**
	 * Returns the current worker. */
	public Worker currentWorker() {
		return currentWorker.get();
	}
	
	/**
	 * Returns a small integer from 0 to {@link #numWorkers}.
	 * This number can change before and after inline subintervals,
	 * but you are always guaranteed that no two concurrent
	 * intervals will read the same results. 
	 * 
	 * @return the id of the current medallion, or -1 if not in an
	 * interval thread */
	public int currentId() {
		Worker worker = currentWorker.get();
		if(worker == null)
			return -1;
		return worker.medallion.id;
	}
	
	/**
	 * Yields the medallion held by the current thread, if any. 
	 * 
	 * @return true if the current thread held a medallion */
	public boolean yieldMedallion() {
		Worker worker = currentWorker.get();
		if(worker == null) {
			return false;
		} else {
			worker.yieldMedallion();
			return true;
		}
	}

	/**
	 * Re-acquires a medallion for the current thread
	 * after it invoked {@link #yieldMedallion()}. */
	public void reacquireMedallion() {
		Worker worker = currentWorker.get();
		assert worker != null && worker.medallion == null;
		worker.reacquireMedallion();
	}

	/**
	 * Submits {@code item} for eventual execution. */
	public void submit(WorkItem item) {		
		Worker worker = currentWorker.get();
		if(worker != null) {
			assert worker.medallion != null;
			worker.medallion.enqueue(item);
		} else {
			Medallion medallion;
			
			synchronized(ThreadPool.this) {
				if(freeMedallion == null) {
					// No one waiting to take this job.  
					// Put it on the list of pending items, and
					// someone will get to it rather than becoming idle.
					pendingWorkItems.add(item);
					
					if(Debug.ENABLED)
						Debug.debug.postAddPendingTask(item);
					
					return;
				} else {
					// There is a free medallion.  Spin up a new worker.
					medallion = freeMedallion;
					freeMedallion = medallion.nextFree;
				}
			}
			
			// Only get here if there was a free medallion.
			startWorkerWith(item, medallion);
		}
	}

}
