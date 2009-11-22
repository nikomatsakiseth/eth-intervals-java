package ch.ethz.intervals.visualizer;

import java.util.ArrayList;
import java.util.List;

public class IdMap<V> {

	private List<V> data = new ArrayList<V>();
	
	public boolean containsKey(int idx) {
		return get(idx) != null;
	}
	
	public V get(int idx) {
		if(idx >= data.size())
			return null;
		return data.get(idx);
	}
	
	public void set(int idx, V value) {
		while(idx >= data.size())
			data.add(null);
		data.set(idx, value);
	}
	
}
