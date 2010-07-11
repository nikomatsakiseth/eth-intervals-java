package ch.ethz.intervals.debug;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.impl.Debug;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;

import com.smallcultfollowing.lathos.JettyLathosServer;
import com.smallcultfollowing.lathos.LathosServer;

public class Basic {

	public static void main(String args[]) 
	throws Exception 
	{
		LathosServer server = JettyLathosServer.start(8080);
		
		server.registerPage(Debug.debug);
		
		Intervals.inline(new AbstractTask("root") {
			@Override
			public void run(Interval current) throws Exception {
				Interval a = current.newAsyncChild(new EmptyTask("a"));
				Interval b = current.newAsyncChild(new EmptyTask("b"));
				Interval c = current.newAsyncChild(new EmptyTask("c"));
				
				a.getEnd().addHb(c.getStart());
				b.getEnd().addHb(c.getStart());
			}
		});
		
		server.join();
	}
	
}
