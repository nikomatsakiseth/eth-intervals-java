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

import jgfmt.jgfutil.*;

public class JGFForkJoinBench implements JGFSection1 {

	public static int nthreads;
	private static final int INITSIZE = 1000;
	private static final int MAXSIZE = 1000000;
	private static final double TARGETTIME = 10.0;

	public JGFForkJoinBench(int nthreads) {
		this.nthreads = nthreads;
	}

	public void JGFrun() {

		int i, size;
		double time, time2;
		time = 0.0;
		time2 = 0.0;
		size = INITSIZE;

		Runnable thobjects[] = new Runnable[nthreads];
		Thread th[] = new Thread[nthreads];

		JGFInstrumentor.addTimer("Section1:ForkJoin:Simple", "forkjoins");

		while (time < TARGETTIME && size < MAXSIZE) {
			JGFInstrumentor.resetTimer("Section1:ForkJoin:Simple");
			JGFInstrumentor.startTimer("Section1:ForkJoin:Simple");

			for (int k = 0; k < size; k++) {

				for (i = 1; i < nthreads; i++) {
					thobjects[i] = new ForkJoinThread(i);
					th[i] = new Thread(thobjects[i]);
					th[i].start();
				}

				thobjects[0] = new ForkJoinThread(0);
				thobjects[0].run();

				for (i = 1; i < nthreads; i++) {
					try {
						th[i].join();
					} catch (InterruptedException e) {
					}
				}

			}

			JGFInstrumentor.stopTimer("Section1:ForkJoin:Simple");
			time = JGFInstrumentor.readTimer("Section1:ForkJoin:Simple");
			JGFInstrumentor.addOpsToTimer(
					"Section1:ForkJoin:Simple",
					(double) size);
			size *= 2;
		}

		size /= 2;

		// now subtract the cost of the run method itself

		JGFInstrumentor.addTimer("Section1:SerialForkJoin:Simple", "forkjoins");

		JGFInstrumentor.startTimer("Section1:SerialForkJoin:Simple");

		for (int k = 0; k < size; k++) {
			thobjects[0] = new ForkJoinThread(0);
			thobjects[0].run();
		}

		JGFInstrumentor.stopTimer("Section1:SerialForkJoin:Simple");

		time2 = JGFInstrumentor.readTimer("Section1:SerialForkJoin:Simple");

		JGFInstrumentor.addTimeToTimer("Section1:ForkJoin:Simple", -time2);
		JGFInstrumentor.printperfTimer("Section1:ForkJoin:Simple");

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
		JGFForkJoinBench fj = new JGFForkJoinBench(nthreads);
		fj.JGFrun();

	}

}

class ForkJoinThread implements Runnable {

	int id;
	long j;

	public ForkJoinThread(int id) {
		this.id = id;
	}

	public void run() {

		// do something trivial but which won't be optimised away!

		double theta = 37.2, sint, res;

		sint = Math.sin(theta);
		res = sint * sint;

		// defeat dead code elimination
		if (res <= 0)
			System.out.println("Benchmark exited with unrealistic res value "
					+ res);

	}
}
