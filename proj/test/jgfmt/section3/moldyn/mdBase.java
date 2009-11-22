package jgfmt.section3.moldyn;

public abstract class mdBase {
	
	int size;
	int mm;
	int datasizes[] = { 8, 13 };

	public static int PARTSIZE;

	public void initialise() {

		mm = datasizes[size];
		PARTSIZE = mm * mm * mm * 4;

	}

	public abstract void runiters();

	public abstract double getresult();

	public abstract double getinteractions();
}
