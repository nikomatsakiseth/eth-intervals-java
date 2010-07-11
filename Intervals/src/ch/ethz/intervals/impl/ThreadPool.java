package ch.ethz.intervals.impl;

import java.util.ArrayList;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicReferenceArray;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

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
	}
	
	static abstract class WorkItem {
		abstract void exec(Medallion medallion);
	}
	
	final class Worker
	implements Runnable
	{
		private Medallion medallion;
		final Semaphore semaphore = new Semaphore(0);
		Worker nextWaiting;
		
		Worker(Medallion initial) {
			medallion = initial;
		}
		
		@Override 
		public void run() {
			while(doWork() && (waitingWorker == null))
				;
			
			releaseMedallion();
		}

		private void acquireMedallion() {
			idleLock.lock();
			Medallion theMedallion = freeMedallion;
			if(theMedallion != null) {
				freeMedallion = theMedallion.nextFree;
			} else {
				nextWaiting = waitingWorker;
				waitingWorker = this;
			}
			idleLock.unlock();

			if(theMedallion != null) {
				theMedallion.nextFree = null;
				medallion = theMedallion;
				return;
			} else {
				// Block until someone wakes us up.
				// They will have assigned us a medallion.
				semaphore.acquireUninterruptibly();
				assert medallion != null;
			}
		}
		
		private void releaseMedallion() {
			Worker awaken;
			
			idleLock.lock();
			awaken = waitingWorker;
			if(awaken == null) {
				medallion.nextFree = freeMedallion;
				freeMedallion = medallion;
			} else {
				waitingWorker = awaken.nextWaiting;
			}
			idleLock.unlock();
			
			if(awaken != null) {
				awaken.nextWaiting = null;
				awaken.medallion = medallion;
				awaken.semaphore.release();
			}
			
			medallion = null;
		}
		
		/**
		 * Tries to do some work. 
		 * @return true if work was done, false otherwise
		 */
		boolean doWork() {
			WorkItem item;			
			if((item = medallion.takeTask()) == null)
				if((item = stealTask()) == null)
					if((item = findPendingWork()) == null)
						return false;
			
			item.exec(medallion);
			return true;
		}

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
			return item;
		}
	
		private WorkItem findPendingWork() {
			idleLock.lock();
			int l = pendingWorkItems.size();
			if(l != 0) {
				WorkItem item = pendingWorkItems.remove(l - 1);
				idleLock.unlock();
				return item;
			} 
			idleLock.unlock();
			return null;
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
	final Executor executor = Executors.newCachedThreadPool();
	
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
	 * Lock used to protect various data structures. */
	final Lock idleLock = new ReentrantLock();
	
	/**
	 * List of work items that were submitted from the outside 
	 * when there were no available medallions.  
	 * Rarely used and should probably be removed. */
	final ArrayList<WorkItem> pendingWorkItems = new ArrayList<WorkItem>();
	
	/**
	 * Head of the linked list of free medallions.  If
	 * null, then there are no available medallions.
	 * 
	 * Modified only while holding {@link #idleLock}, but 
	 * read without lock. */
	volatile Medallion freeMedallion;

	/**
	 * Head of the linked list of workers waiting for a 
	 * medallion.  If null, then there are no waiting workers.
	 * 
	 * Modified only while holding {@link #idleLock}, but 
	 * read without lock. */
	volatile Worker waitingWorker; 
	
	public ThreadPool() {
		freeMedallion = medallions[0] = new Medallion(0);
		for(int i = 1; i < numWorkers; i++) {
			Medallion medallion = new Medallion(i);
			medallions[i] = medallion;
			medallions[i-1].nextFree = medallion;
		}
	}

	private boolean tryToStartWorkerWith(WorkItem item) {
		Medallion medallion = null;
		
		idleLock.lock();
		medallion = freeMedallion;
		if(medallion != null) {
			freeMedallion = medallion.nextFree;
		}
		idleLock.unlock();
			
		if(medallion == null)
			return false;

		startWorkerWith(item, medallion);
		return false;
	}

	private void startWorkerWith(WorkItem item, Medallion medallion) {
		medallion.nextFree = null;
		medallion.tasks.put(medallion, item);
		executor.execute(new Worker(medallion));
	}
	
	public int currentId() {
		return currentWorker.get().medallion.id;
	}
	
	/**
	 * Yields the medallion held by the current thread,
	 * if any. 
	 * 
	 * @return true if the current thread held a medallion */
	public boolean yieldMedallion() {
		Worker worker = currentWorker.get();
		if(worker == null) {
			return false;
		} else {
			assert worker.medallion != null;
			worker.releaseMedallion();
			return true;
		}
	}
	
	/**
	 * Re-acquires a medallion for the current thread
	 * after it invoked {@link #yieldMedallion()}. */
	public void reacquireMedallion() {
		Worker worker = currentWorker.get();
		assert worker != null && worker.medallion == null;
		worker.acquireMedallion();
	}

	public void submit(WorkItem item) {		
		Worker worker = currentWorker.get();
		if(worker != null) {
			assert worker.medallion != null;
			worker.medallion.enqueue(item);
		} else {
			idleLock.lock();		
			
			if(freeMedallion == null) {
				// No one waiting to take this job.  
				// Put it on the list of pending items, and
				// someone will get to it rather than becoming idle.
				pendingWorkItems.add(item);
				idleLock.unlock();
			} else {
				// There is a free medallion.  Spin up a new worker.
				Medallion medallion = freeMedallion;
				freeMedallion = freeMedallion.nextFree;
				idleLock.unlock();
				startWorkerWith(item, medallion);
			}					
		}
	}

}
