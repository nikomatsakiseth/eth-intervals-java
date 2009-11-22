/**
 * 
 */
package ch.ethz.intervals.visualizer;

class UserEvent {
	final int sourceIndex;
	final String description;
	
	public UserEvent(int sourceIndex, String description) {
		this.sourceIndex = sourceIndex;
		this.description = description;
	}
	
	@Override public String toString() {
		return this.description;
	}
}