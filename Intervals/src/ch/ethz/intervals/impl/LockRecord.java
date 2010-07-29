package ch.ethz.intervals.impl;

import java.io.IOException;

import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;
import com.smallcultfollowing.lathos.PageContent;

import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.guard.Guard;

/**
 * A little data structure that tracks which
 * locks are held and by whom.  
 * 
 * <p>What makes this data structure a bit complex
 * is that it simultaneously sits on up to three linked
 * lists:
 * <ul>
 * <li> The interval acquiring this lock has a linked
 *      list using {@link #nextForInterval} as the
 *      next pointer.
 * <li> The point acquiring the lock has a linked list
 *      using {@link #nextForAcquirer} as the next pointer.
 * <li> The point releasing the lock has a linked list
 *      using {@link #nextForReleaser} as the next pointer. 
 * </ul>
 * The last two linked lists are a bit special, because 
 * the next pointer points to the next lock record --- but
 * the point may play a different role in the next record.
 * The method {@link #nextFor(PointImpl)} helps to keep it
 * all straight.
 * 
 * <p>Note: this is the only instance in the implementation
 * where a data structure potentially has a pointer back
 * in time.  In this case, if {@link #acquiredBy} is 
 * not equal to the start point of the interval, then
 * the interval will have a pointer back in time.  But
 * it can't really be helped, as the user must be able
 * to check and validate the point at which the lock has
 * been acquired.
 */
public class LockRecord 
implements Page 
{
	private final static int FLAG_ACQUIRED = 1;
	private final static int FLAG_RECURSIVE = 2;
	
	public final LockImpl lock;
	public final Guard guard;
	public final PointImpl acquiredBy;
	public final PointImpl releasedBy;
	
	private int flags = 0;
	private LockRecord nextForInterval;
	private LockRecord nextForAcquirer;
	private LockRecord nextForReleaser;
	
	public LockRecord(
			LockImpl lock, 
			Guard guard, 
			PointImpl acquiredBy,
			PointImpl releasedBy
	) {
		this.lock = lock;
		this.guard = guard;
		this.acquiredBy = acquiredBy;
		this.releasedBy = releasedBy;
	}

	/** True if neither acquired nor recursive */
	public boolean mustAcquire() {
		return (flags & (FLAG_ACQUIRED|FLAG_RECURSIVE)) == 0;
	}
	
	public boolean isAcquired() {
		return (flags & FLAG_ACQUIRED) != 0;
	}
	
	public void setAcquired() {
		flags |= FLAG_ACQUIRED;
	}
		
	public boolean isRecursive() {
		return (flags & FLAG_RECURSIVE) != 0;
	}
	
	public void setRecursive() {
		flags |= FLAG_RECURSIVE;
	}

	public boolean isAcquirer(PointImpl point) {
		return acquiredBy == point;
	}
	
	public boolean isReleaser(PointImpl point) {
		return releasedBy == point;
	}
	
	public LockRecord insertFor(PointImpl point, LockRecord next) {
		if(point == acquiredBy) {
			assert nextForAcquirer == null;
			nextForAcquirer = next;
		} else {
			assert point == releasedBy;
			assert nextForReleaser == null;
			nextForReleaser = next;
		}
		return this;
	}
	
	public LockRecord nextFor(PointImpl point) {
		if(point == acquiredBy)
			return nextForAcquirer;
		else
			return nextForReleaser;
	}
	
	public LockRecord insertForInterval(LockRecord next) {
		assert nextForInterval == null;
		nextForInterval = next;
		return this;
	}
	
	public LockRecord nextForInterval() {
		return nextForInterval;
	}

	/** True if this record acquires {@code aLock} for {@code aGuard}.
	 *  If {@code aGuard} is null then true if the record acquires 
	 *  {@code aLock} for any guard. */
	public boolean matches(RoLock aLock, Guard aGuard) {
		return (lock == aLock) && (guard == aGuard || aGuard == null);
	}

	public void releaseIfAcquired() {
		if(isAcquired()) {
			flags &= ~FLAG_ACQUIRED;
			lock.release(this);
		}
	}

	@Override
	public void renderInPage(Output out) throws IOException {
		Lathos.reflectivePage(this, out);
	}

	@Override
	public void renderInLine(Output output) throws IOException {
		Lathos.renderInLine(this, output);
	}

	@Override
	public String getId() {
		return Lathos.defaultId(this);
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
