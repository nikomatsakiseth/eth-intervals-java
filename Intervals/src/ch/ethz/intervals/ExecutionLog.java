package ch.ethz.intervals;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.JFrame;

import ch.ethz.intervals.visualizer.EventLog;
import ch.ethz.intervals.visualizer.EventLogVisitor;
import ch.ethz.intervals.visualizer.EventProducerThread;
import ch.ethz.intervals.visualizer.VisualizerWindow;
import ch.ethz.intervals.visualizer.EventLog.Arrive;
import ch.ethz.intervals.visualizer.EventLog.Edge;
import ch.ethz.intervals.visualizer.EventLog.Event;
import ch.ethz.intervals.visualizer.EventLog.Lock;
import ch.ethz.intervals.visualizer.EventLog.NewInterval;
import ch.ethz.intervals.visualizer.EventLog.Schedule;


/**
 * The Execution Log is a utility for debugging intervals.
 * The intended usage is that you invoke {@link #enable()}
 * or {@link #enableGui()} before you create some troublesome
 * set of intervals, and then later invoke {@link #disable()}.
 * Interval events between the two calls will then be logged.
 */
public class ExecutionLog {

	static volatile ExecutionLog log = null;
	
	/**
	 * Logs events into a temporary file (named {@code Intervals.*.executionLog})
	 * for later, postmortem analysis.
	 */
	public static void enable() {
		synchronized(ExecutionLog.class) {
			if(log == null) {
				log = new ExecutionLog();
				log.logToDisk();
			}
		}
	}
	
	/**
	 * Creates a visualization window and feeds it events live.  The window
	 * will be updated as the program executes.
	 */
	public static void enableGui() {
		synchronized(ExecutionLog.class) {
			if(log == null) {
				log = new ExecutionLog();
				log.logToGui();
			}
		}
	}	
	
	/**
	 * Convenience wrapper that
	 * enables the GUI, invokes {@link Runnable#run()}, 
	 * and then disables the GUI. 
	 */
	public static void runWithGui(Runnable run) {
		enableGui();
		try {
			run.run();
		} finally {
			disable();
		}
	}	
	
	/** 
	 * Disables the execution log.  Blocks until the gui window (if any)
	 * is closed.
	 */
	public static void disable() {
		ExecutionLog l;
		synchronized(ExecutionLog.class) {
			l = log;
			log = null;
		}
		
		if(l != null) {				
			l.blockTillComplete();
		}
	}
	
	// Returns something addable to a String that uniquely identities 'i'.
	//   (identityHashCode() is not perfect, but it's close enough I guess)
	private int identity(Object i) { 
		return System.identityHashCode(i);
	}
	
	static void logNewInterval(Point current, Point sp, Point ep) {
		ExecutionLog l = log;
		if(l != null) {
			l.events.add(new EventLog.NewInterval(
					l.identity(current),
					l.identity(sp),
					l.identity(ep),
					l.identity(ep.bound())));
		}
	}
	
	static void logScheduleInterval(Point sp, Task t) {
		ExecutionLog l = log;
		if(l != null) {
			l.events.add(new EventLog.Schedule(l.identity(sp), t.toString().replace('\n', ' ')));			
		}
	}

	static void logEdge(Point from, Point to) {
		ExecutionLog l = log;
		if(l != null) {
			l.events.add(new EventLog.Edge(l.identity(from), l.identity(to)));
		}
	}
	
	static void logArrive(Point pnt) {
		ExecutionLog l = log;
		if(l != null) {
			l.events.add(new EventLog.Arrive(l.identity(pnt)));
		}
	}
	
	static void logLock(Point i, Guard g) {
		ExecutionLog l = log;
		if(l != null) {
			l.events.add(new EventLog.Lock(l.identity(i), l.identity(g)));
		}
	}
	
	private void logToDisk() {
		logToDiskThread = new LogToDiskThread();
		logToDiskThread.start();
	}
	
	private void logToGui() {
		gui = new GuiKeepAlive();
		
		VisualizerWindow window = new VisualizerWindow();	
		window.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		window.addWindowListener(gui);
		
		gui.lep = new LiveEventProducer(window);
		gui.lep.execute();
		
		gui.start();
	}
	
	private void blockTillComplete() {
		events.add(EventLog.STOP_LOGGING);
		
		if(logToDiskThread != null) {
			try {
				logToDiskThread.join();
			} catch (InterruptedException e) { }
			logToDiskThread = null;
		}
		
		if(gui != null) {
			try {
				gui.join();
			} catch (InterruptedException e) { }				
			gui = null;
		}		
	}

	private BlockingQueue<EventLog.Event> events = new LinkedBlockingQueue<EventLog.Event>();	
	private GuiKeepAlive gui;	
	private LogToDiskThread logToDiskThread; 
	
	class LogToDiskThread extends Thread {
		public LogToDiskThread() {
			setDaemon(true);
		}
		
		@Override
		public void run() {
			try {
				FileWriter out = null;
				try {
					File tmpFile = File.createTempFile("Intervals.", ".executionLog");
					out = new FileWriter(tmpFile);
					while(true) {
						Event s;
						try {
							s = events.take();
						} catch (InterruptedException e) {
							s = EventLog.STOP_LOGGING;
						}
						
						if (s == EventLog.STOP_LOGGING)
							break;
						out.write(s.toString());
						out.write("\n");
						out.flush();
					}
				} catch(IOException e) {
					e.printStackTrace();    // What can we do?  No logging now!
				}
				
				// Close the file.
				if(out != null) {
					try {
						out.close();
					} catch (IOException e) { // What can we do?
					}
				}
			} finally {
				synchronized(ExecutionLog.class) {
					log = null; // should eventually cause the queue to be garbaged collected etc
				}
			}
		}
	}
	
	class GuiKeepAlive 
	extends Thread
	implements WindowListener
	{
		CountDownLatch latch = new CountDownLatch(1);
		LiveEventProducer lep;
		
		@Override
		public void run() {
			try {
				lep.get();
				latch.await();
			} catch (InterruptedException e) {
			} catch (ExecutionException e) {
			}
		}

		@Override
		public void windowActivated(WindowEvent e) {
		}

		@Override
		public void windowClosed(WindowEvent e) {
			latch.countDown();
		}

		@Override
		public void windowClosing(WindowEvent e) {
		}

		@Override
		public void windowDeactivated(WindowEvent e) {
		}

		@Override
		public void windowDeiconified(WindowEvent e) {
		}

		@Override
		public void windowIconified(WindowEvent e) {
		}

		@Override
		public void windowOpened(WindowEvent e) {
		}
	}
	
	class LiveEventProducer 
	extends EventProducerThread
	implements EventLogVisitor
	{
		ShortenMap<Integer> shortenMap = new ShortenMap<Integer>();
		
		public LiveEventProducer(VisualizerWindow window) {
			super(window);
		}

		@Override
		protected Void doInBackground() throws Exception {
			while(true){
				try {
					Event s = events.take();
					if(s == EventLog.STOP_LOGGING)
						break;
					s.accept(this, 0);
				} catch(InterruptedException e) {
					break;
				}
			}
			return null;
		}

		@Override
		public void visitArrive(Arrive event, int index) {
			publish(new Arrive(shortenMap.shorten(event.pointId)));
		}

		@Override
		public void visitEdge(Edge event, int index) {
			publish(new Edge(
					shortenMap.shorten(event.fromPointId),
					shortenMap.shorten(event.toPointId)));
		}

		@Override
		public void visitLock(Lock event, int index) {
			publish(new Lock(
					shortenMap.shorten(event.startPointId),
					shortenMap.shorten(event.lockId)));
		}

		@Override
		public void visitNewInterval(NewInterval event, int index) {
			publish(new NewInterval(
					shortenMap.shorten(event.currentId),
					shortenMap.introduce(event.startId),
					shortenMap.introduce(event.endId),
					shortenMap.shorten(event.endBoundId)));
		}

		@Override
		public void visitSchedule(Schedule event, int index) {
			publish(new Schedule(
					shortenMap.shorten(event.startPointId),
					event.taskDescr));
		}
		
	}
}
