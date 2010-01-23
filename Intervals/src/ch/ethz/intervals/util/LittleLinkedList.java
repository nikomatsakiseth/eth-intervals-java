package ch.ethz.intervals.util;

public class LittleLinkedList<X> {
	public final X value;
	public LittleLinkedList<X> next;
	
	public LittleLinkedList(X value) {
		this.value = value;
	}
	
	public LittleLinkedList(X value, LittleLinkedList<X> next) {
		this.value = value;
		this.next = next;
	}
}
