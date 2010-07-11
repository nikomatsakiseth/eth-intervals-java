package ch.ethz.intervals.debug;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.impl.Debug;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;

import com.smallcultfollowing.lathos.JettyLathosServer;
import com.smallcultfollowing.lathos.LathosServer;

// Generates an infinite stream of events, most of which
// are irrelevant.
//
// However, the events related to the task "later"
// never become irrelevant and so should stay.
public class Many {
	
	static class InfiniteTask extends AbstractTask {
		final long id;
		final Interval later;
		
		InfiniteTask(long id, Interval later) {
			super(Long.toString(id));
			this.id = id;
			this.later = later;
		}

		@Override
		public void run(Interval current) throws Exception {
			Interval next = current.getParent().newAsyncChild(new InfiniteTask(id + 1, later));
			next.getEnd().addHb(later.getStart());
		}
	}

	public static void main(String args[]) 
	throws Exception 
	{
		LathosServer server = JettyLathosServer.start(8080);
		
		server.registerPage(Debug.debug);
		
		Intervals.inline(new AbstractTask("root") {
			@Override
			public void run(Interval current) throws Exception {
				Interval later = current.newAsyncChild(new EmptyTask("later"));
				Interval inf = current.newAsyncChild(new InfiniteTask(0, later));
				inf.getEnd().addHb(later.getStart());
			}
		});
		
		server.join();
	}
	
}
