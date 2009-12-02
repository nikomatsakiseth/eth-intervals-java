package jgfmt.section1;

/**************************************************************************
 *                                                                         *
 *         Java Grande Forum Benchmark Suite - Thread Version 1.0          *
 *                                                                         *
 *                            produced by                                  *
 *                                                                         *
 *                  Java Grande Benchmarking Project                       *
 *                                                                         *
 *                                at                                       *
 *                                                                         *
 *                Edinburgh Parallel Computing Centre                      *
 *                                                                         * 
 *                email: epcc-javagrande@epcc.ed.ac.uk                     *
 *                                                                         *
 *                                                                         *
 *      This version copyright (c) The University of Edinburgh, 2001.      *
 *                         All rights reserved.                            *
 *                                                                         *
 **************************************************************************/

import jgfmt.jgfutil.JGFInstrumentor;
import jgfmt.jgfutil.JGFSection1;

public class JGFBarrierBench implements JGFSection1 {

	public static int nthreads;
	private static final int INITSIZE = 10000;
	private static final int MAXSIZE = 10000000;
	private static final double TARGETTIME = 10.0;

	public JGFBarrierBench(int nthreads) {
		this.nthreads = nthreads;
	}

	public void JGFrun() {

		int i, size;
		double time;

		time = 0.0;
		size = INITSIZE;
		int shared_cont = 0;

		Runnable thobjects[] = new Runnable[nthreads];
		Thread th[] = new Thread[nthreads];
		Counter cont = new Counter(shared_cont);
		Barrier br = new TournamentBarrier(nthreads);

		JGFInstrumentor.addTimer("Section1:Barrier:Simple", "barriers");

		while (time < TARGETTIME && size < MAXSIZE) {
			JGFInstrumentor.resetTimer("Section1:Barrier:Simple");
			JGFInstrumentor.startTimer("Section1:Barrier:Simple");

			for (i = 0; i < nthreads; i++) {
				thobjects[i] = new SimpleBarrierThread(i, cont, size);
				th[i] = new Thread(thobjects[i]);
				th[i].start();
			}

			for (i = 0; i < nthreads; i++) {
				try {
					th[i].join();
				} catch (InterruptedException e) {
				}
			}

			JGFInstrumentor.stopTimer("Section1:Barrier:Simple");
			time = JGFInstrumentor.readTimer("Section1:Barrier:Simple");
			JGFInstrumentor.addOpsToTimer(
					"Section1:Barrier:Simple",
					(double) size);
			size *= 2;
		}

		JGFInstrumentor.printperfTimer("Section1:Barrier:Simple");

		JGFInstrumentor.addTimer("Section1:Barrier:Tournament", "barriers");

		time = 0.0;
		size = INITSIZE;

		while (time < TARGETTIME && size < MAXSIZE) {
			JGFInstrumentor.resetTimer("Section1:Barrier:Tournament");
			JGFInstrumentor.startTimer("Section1:Barrier:Tournament");

			for (i = 0; i < nthreads; i++) {
				thobjects[i] = new TournamentBarrierThread(i, br, size);
				th[i] = new Thread(thobjects[i]);
				th[i].start();
			}

			for (i = 0; i < nthreads; i++) {
				try {
					th[i].join();
				} catch (InterruptedException e) {
				}
			}

			JGFInstrumentor.stopTimer("Section1:Barrier:Tournament");
			time = JGFInstrumentor.readTimer("Section1:Barrier:Tournament");
			JGFInstrumentor.addOpsToTimer(
					"Section1:Barrier:Tournament",
					(double) size);
			size *= 2;
		}

		JGFInstrumentor.printperfTimer("Section1:Barrier:Tournament");

	}

	public static void main(String[] argv) {

		if (argv.length != 0) {
			nthreads = Integer.parseInt(argv[0]);
		} else {
			System.out
					.println("The no of threads has not been specified, defaulting to 1");
			System.out.println("  ");
			nthreads = 1;
		}

		JGFInstrumentor.printHeader(1, 0, nthreads);

		JGFBarrierBench tb = new JGFBarrierBench(nthreads);
		tb.JGFrun();

	}

}

class Counter {
	int shared_cont;

	public Counter(int shared_cont) {
		this.shared_cont = shared_cont;
	}
}

class SimpleBarrierThread implements Runnable {

	int id, size;
	Counter cont;

	public SimpleBarrierThread(int id, Counter cont, int size) {
		this.id = id;
		this.cont = cont;
		this.size = size;
	}

	public void run() {

		for (int i = 0; i < size; i++) {
			synchronized (cont) {
				cont.shared_cont++;

				try {
					if (cont.shared_cont != JGFBarrierBench.nthreads) {
						cont.wait();

					} else {
						cont.shared_cont = 0;
						cont.notifyAll();
					}

				} catch (InterruptedException e) {
				}
			}

		}
	}

}

class TournamentBarrierThread implements Runnable {

	int id, size;
	Barrier br;

	public TournamentBarrierThread(int id, Barrier br, int size) {
		this.id = id;
		this.br = br;
		this.size = size;
	}

	public void run() {
		for (int i = 0; i < size; i++)
			br.DoBarrier(id);
	}

}
