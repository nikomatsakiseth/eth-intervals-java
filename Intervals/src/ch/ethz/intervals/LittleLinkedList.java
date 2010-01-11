package ch.ethz.intervals;

class LittleLinkedList<X> {
	final X value;
	LittleLinkedList<X> next;
	
	LittleLinkedList(X value) {
		this.value = value;
	}
	
	LittleLinkedList(X value, LittleLinkedList<X> next) {
		this.value = value;
		this.next = next;
	}
}
