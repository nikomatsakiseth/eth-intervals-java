package jgfmt.section2;

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
import jgfmt.section2.series.JGFSeriesBench;

public class JGFSeriesBenchSizeB {

	public static int nthreads;

	public static void main(String argv[]) {

		if (argv.length != 0) {
			nthreads = Integer.parseInt(argv[0]);
		} else {
			System.out
					.println("The no of threads has not been specified, defaulting to 1");
			System.out.println("  ");
			nthreads = 1;
		}

		JGFInstrumentor.printHeader(2, 1, nthreads);

		JGFSeriesBench se = new JGFSeriesBench(nthreads);
		se.JGFrun(1);

	}
}
