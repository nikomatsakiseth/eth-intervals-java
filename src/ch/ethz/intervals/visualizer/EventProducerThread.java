package ch.ethz.intervals.visualizer;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.SwingWorker;

import ch.ethz.intervals.visualizer.EventLog.Event;

public abstract class EventProducerThread extends SwingWorker<Void, Event> {

	/**
	 * Interval identifiers must be unique but may be reused after a 
	 * NEW_INTERVAL.  Furthermore, they tend to be big long ugly strings.
	 * This class converts them to truly unique small integers.
	 */
	public class ShortenMap<K> {
		private final Map<K, Integer> map = new HashMap<K, Integer>();
		
		public int introduce(K id) {
			int newId = map.size() + 1;
			map.put(id, newId);
			return newId;
		}
		
		public int shorten(K id) {
			Integer newId = map.get(id); 
			if(newId == null) { // shouldn't happen, but try to be robust
				return 0; // 0 == unrecognized
			}
			return newId;
		}
	}
	
	protected final VisualizerWindow window;
	
	public EventProducerThread(VisualizerWindow window) {
		this.window = window;
	}

	@Override
	protected void process(List<Event> chunks) {
		super.process(chunks);		
		for(Event event : chunks)
			window.processNewEvent(event);
	}	

	@Override
	protected void done() {
		super.done();		
	}	

}
