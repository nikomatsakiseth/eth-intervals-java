package ch.ethz.intervals;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class TestGuardImpl {
	
	@Mock IntervalImpl fst;
	@Mock IntervalImpl snd;
	@Mock IntervalImpl thrd;
	
	@Before public void before() {
	
	}
	
	@Test public void foo() {
		
	}
	/*
	@Test public void siblingsBothClaimingExclusiveAccess() {
		Mockito.when(snd.isDescendantOf(fst)).thenReturn(false);
		
		GuardImpl g = Mockito.spy(GuardImpl.HEAP.newGuard());
		Assert.assertEquals(1, g.tryClaimExclusively(fst));
		Assert.assertEquals(0, g.tryClaimExclusively(snd));
		g.release();
		
		Mockito.verify(fst, Mockito.never()).lockAcquired();
		Mockito.verify(snd).lockAcquired();
	}

	@Test public void parentChildSiblingAllClaimingExclusiveAccess() {
		Mockito.when(snd.isDescendantOf(fst)).thenReturn(true);
		Mockito.when(thrd.isDescendantOf(fst)).thenReturn(false);
		
		GuardImpl g = Mockito.spy(GuardImpl.HEAP.newGuard());
		Assert.assertEquals(1, g.tryClaimExclusively(fst));
		Assert.assertEquals(0, g.tryClaimExclusively(thrd));
		Assert.assertEquals(0, g.tryClaimExclusively(snd));
		g.halfRelease(); // half-release fst's lock: snd, its descendant, now aquires the lock
		g.release(); // release snd's lock: lock state for snd is popped, but thrd cannot yet acquire the lock
		g.release(); // release fst's lock: NOW thrd can acquire the lock
		
		Mockito.verify(fst, Mockito.never()).lockAcquired();
		
		InOrder inOrder = Mockito.inOrder(fst, snd, thrd, g);
		inOrder.verify(g).halfRelease();
		inOrder.verify(snd).lockAcquired();
		inOrder.verify(g, times(2)).release();
		inOrder.verify(thrd).lockAcquired();
	}

	@Test public void parentChildSiblingClaimingExclusiveAndSharedAccess() {
		Mockito.when(snd.isDescendantOf(fst)).thenReturn(true);
		Mockito.when(thrd.isDescendantOf(fst)).thenReturn(false);
		
		GuardImpl g = Mockito.spy(GuardImpl.HEAP.newGuard());
		Assert.assertEquals(1, g.tryClaimExclusively(fst));
		Assert.assertEquals(0, g.tryClaimExclusively(thrd));
		Assert.assertEquals(0, g.tryClaimExclusively(snd));
		g.halfRelease(); // half-release fst's lock: snd, its descendant, now aquires the lock
		Assert.assertEquals(0, g.tryClaimShared(fst)); // snd holds excl., cannot yet obtain
		Assert.assertEquals(0, g.tryClaimShared(fst)); // snd holds excl., cannot yet obtain
		g.halfRelease(); // half-release snd's lock: helps no one as no desc. of snd are pending
		g.release(); // release snd's lock: lock state for snd is popped, fst obtains shared access
		g.release(); // release fst's shared lock #1: thrd still cannot acquire the lock
		g.release(); // release fst's shared lock #2: NOW thrd can acquire the lock
		
		InOrder inOrder = Mockito.inOrder(fst, snd, thrd, g);
		inOrder.verify(g).halfRelease();
		inOrder.verify(snd).lockAcquired();
		inOrder.verify(g).halfRelease();
		inOrder.verify(g).release();
		inOrder.verify(fst, times(2)).lockAcquired();
		inOrder.verify(g, times(2)).release();
		inOrder.verify(thrd).lockAcquired();
	}

	@Test public void drainSharedAccess() {
		GuardImpl g = Mockito.spy(GuardImpl.HEAP.newGuard());
		Assert.assertEquals(1, g.tryClaimExclusively(fst));
		g.halfRelease();
		Assert.assertEquals(1, g.tryClaimShared(fst)); // SHARED: succeeds
		Assert.assertEquals(1, g.tryClaimShared(fst)); // SHARED: succeeds
		Assert.assertEquals(0, g.tryClaimExclusively(fst)); // EXCLUSIVE: fails, pending
		Assert.assertEquals(0, g.tryClaimShared(fst)); // SHARED: fails because exclusive is pending
		g.release(); // release shared #1
		g.release(); // release shared #2, exclusive now acquired
		g.release(); // release exclusive, shared now acquired
		
		InOrder inOrder = Mockito.inOrder(fst, snd, thrd, g);
		inOrder.verify(g).tryClaimExclusively(fst);
		inOrder.verify(g).halfRelease();
		inOrder.verify(g).tryClaimShared(fst);
		inOrder.verify(g).tryClaimShared(fst);
		inOrder.verify(g).tryClaimExclusively(fst);
		inOrder.verify(g).tryClaimShared(fst);
		inOrder.verify(g, times(2)).release();
		inOrder.verify(fst).lockAcquired();
		inOrder.verify(g).release();
		inOrder.verify(fst).lockAcquired();
	}*/
	
}
