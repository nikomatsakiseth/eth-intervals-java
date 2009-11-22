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
 *      Original version of this code by Hon Yau (hwyau@epcc.ed.ac.uk)     *
 *                                                                         *
 *      This version copyright (c) The University of Edinburgh, 2001.      *
 *                         All rights reserved.                            *
 *                                                                         *
 **************************************************************************/

package jgfmt.section3.montecarlo;

/**
 * Wrapper code to invoke the Application demonstrator.
 * 
 * @author H W Yau
 * @version $Revision: 1.19 $ $Date: 1999/02/16 19:10:02 $
 */
public class CallAppDemo {
	public int size;
	int datasizes[] = { 10000, 60000 };
	int input[] = new int[2];
	AppDemoInterface ap = null;

	public void initialise() {

		input[0] = 1000;
		input[1] = datasizes[size];

		String dirName = "test/jgfmt/section3/Data";
		String filename = "hitData";
		
		if (JGFMonteCarloBench.nthreads != -1)
			ap = new AppDemo(dirName, filename, (input[0]), (input[1]));
		else
			ap = new AppDemoIntervals(dirName, filename, (input[0]), (input[1]));
		
		ap.initSerial();
	}

	public void runiters() {
		ap.runThread();
	}

	public void presults() {
		ap.processSerial();
	}

}
