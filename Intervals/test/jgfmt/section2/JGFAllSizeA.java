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
import jgfmt.section2.crypt.JGFCryptBench;
import jgfmt.section2.lufact.JGFLUFactBench;
import jgfmt.section2.series.JGFSeriesBench;
import jgfmt.section2.sor.JGFSORBench;
import jgfmt.section2.sparsematmult.JGFSparseMatmultBench;

public class JGFAllSizeA {

	public static int nthreads;

	public static void main(String argv[]) {

		int size = 0;

		if (argv.length != 0) {
			nthreads = Integer.parseInt(argv[0]);
		} else {
			System.out
					.println("The no of threads has not been specified, defaulting to 1");
			System.out.println("  ");
			nthreads = 1;
		}

		JGFInstrumentor.printHeader(2, size, nthreads);

		JGFSeriesBench se = new JGFSeriesBench(nthreads);
		se.JGFrun(size);

		JGFLUFactBench lub = new JGFLUFactBench(nthreads);
		lub.JGFrun(size);

		JGFCryptBench cb = new JGFCryptBench(nthreads);
		cb.JGFrun(size);

		JGFSORBench jb = new JGFSORBench(nthreads);
		jb.JGFrun(size);

		JGFSparseMatmultBench smm = new JGFSparseMatmultBench(nthreads);
		smm.JGFrun(size);

	}
}
