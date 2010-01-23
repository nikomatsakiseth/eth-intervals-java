package ch.ethz.intervals.visualizer;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileReader;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Scanner;

import javax.swing.ProgressMonitor;
import javax.swing.SwingWorker;

import ch.ethz.intervals.visualizer.EventLog.AddLock;
import ch.ethz.intervals.visualizer.EventLog.Arrive;
import ch.ethz.intervals.visualizer.EventLog.Edge;
import ch.ethz.intervals.visualizer.EventLog.NewInterval;
import ch.ethz.intervals.visualizer.EventLog.Schedule;

public class EventLogParser extends EventProducerThread {
	
	public static SwingWorker<?, ?> startParse(VisualizerWindow window, File file) {
	    final ProgressMonitor parseMonitor = new ProgressMonitor(window, "Loading event log", null, 0, 100);
    	parseMonitor.setMillisToPopup(0);
    	parseMonitor.setMillisToDecideToPopup(0);	
    	
    	EventLogParser elp = new EventLogParser(window, file);

		elp.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("progress")) {
					parseMonitor.setProgress((Integer)evt.getNewValue());
				}
			}
		});
		
		elp.execute();   
		
		return elp;
	}
	
	private final File file;
	private final List<String> warnings = new ArrayList<String>();
	
	public EventLogParser(VisualizerWindow window, File file) {
		super(window);
		this.file = file;
	}

	@Override
	protected Void doInBackground() throws Exception {		
		long processed = 0, fileLength = file.length();
		LineNumberReader reader = new LineNumberReader(new FileReader(file));
		ShortenMap<String> shorten = new ShortenMap<String>();
		String line;
		while(!isCancelled() && (line = reader.readLine()) != null) {
			Scanner scan = new Scanner(line);
			String tag = scan.next();
			try {
				if(tag.equals("NEW_INTERVAL")) {
					publish(new NewInterval(
						shorten.shorten(scan.next()),  
						shorten.introduce(scan.next()),
						shorten.introduce(scan.next()),
						shorten.shorten(scan.next())));
				} else if(tag.equals("SCHEDULE")) {
					publish(new Schedule(
						shorten.shorten(scan.next()), 
						scan.next(".*")));
				} else if(tag.equals("EDGE")) {
					int fromId = shorten.shorten((scan.next()));
					int toId = shorten.shorten(scan.next());
					publish(new Edge(fromId, toId));
				} else if(tag.equals("ARRIVE")) {
					int id = shorten.shorten(scan.next());
					publish(new Arrive(id));
				} else if(tag.equals("LOCK")) {
					int iid = shorten.shorten(scan.next());
					int gid = shorten.shorten(scan.next());
					publish(new AddLock(iid, gid));
				} else {
					warnings.add(String.format(
							"%s:%d: unrecognized tag '%s'", 
							file, reader.getLineNumber(), tag));
				}
			} catch (NoSuchElementException e) {
				warnings.add(String.format(
						"%s:%d: unexpected end-of-line", file, reader.getLineNumber()));
			} catch (IllegalArgumentException e) {
				warnings.add(String.format(
						"%s:%d: side should be START or END", 
						file, reader.getLineNumber()));				
			}
			
			processed += line.length() + 1; // (1 for the \n)
			
			setProgress((int)(100 * processed / fileLength));
		}
		return null;
	}

}
