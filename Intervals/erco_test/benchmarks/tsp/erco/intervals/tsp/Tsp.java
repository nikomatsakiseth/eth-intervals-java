package erco.intervals.tsp;

/*
 * Copyright (C) 2000 by ETHZ/INF/CS
 * All rights reserved
 * 
 * @version $Id: Tsp.java 2094 2003-01-30 09:41:18Z praun $
 * @author Florian Schneider
 */

import java.util.*;
import java.io.*;

import ch.ethz.intervals.Dependency;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.ParentForNew;
import ch.ethz.intervals.VoidInlineTask;

public class Tsp extends Interval {
	public final static boolean debug = false;
	public final static int MAX_TOUR_SIZE = 32;
	public final static int MAX_NUM_TOURS = 5000;
	public final static int BIGINT = 2000000;
	public final static int END_TOUR = -1;
	public final static int ALL_DONE = -1;
	static int nWorkers = 2;
	static int TspSize = MAX_TOUR_SIZE;
	static int StartNode = 0;
	static int NodesFromEnd = 12;
	static volatile int routesComputed;
	
	public Tsp(@ParentForNew("Parent") Dependency dep) {
		super(dep, "Tsp");
	}

	public static void main(String[] args) {
		int i;
		String fname = "testdata";
		boolean nop = false;

		try {
			fname = args[0];
			if (fname.equals("--nop"))
				nop = true;
			else
				nWorkers = Integer.parseInt(args[1]);
		} catch (Exception e) {
			System.out
					.println("usage: java Tsp <input file> <number of threads>\n"
							+ "    or java Tsp --nop");
			System.exit(-1);
		}

		// start computation
		System.gc();
		if (!nop) {

			TspSolver.TourStackTop = -1;
			TspSolver.MinTourLen = BIGINT;

			TspSize = read_tsp(fname);
			
			Intervals.inline(new VoidInlineTask() {				
				@Override public void run(Interval subinterval) {
					new Tsp(subinterval);
				}
			});
		}
	}

	static int read_tsp(String fname) {
		String line;
		StringTokenizer tok;
		int i, j;

		try {
			BufferedReader in = new BufferedReader(new FileReader(fname));
			TspSize = Integer.parseInt(in.readLine());

			for (i = 0; i < TspSize; i++) {
				line = in.readLine();
				tok = new StringTokenizer(line, " ");
				j = 0;
				while (tok.hasMoreElements())
					TspSolver.weights[i][j++] = Integer.parseInt((String) tok
							.nextElement());
			}
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(-1);
		}
		return (TspSize);
	}

	@Override
	protected void run() {
		long start = new Date().getTime();

		Intervals.inline(new VoidInlineTask() {
			@Override public String toString() { return "Tsp.init"; }
			@Override public void run(Interval subinterval) {
				/* init arrays */
				for (int i = 0; i < MAX_NUM_TOURS; i++) {
					TspSolver.Tours[i] = new TourElement();
					TspSolver.PrioQ[i] = new PrioQElement();
				}

				/* Initialize first tour */
				TspSolver.Tours[0].prefix()[0] = StartNode;
				TspSolver.Tours[0].setConn(1);
				TspSolver.Tours[0].setLast(0);
				TspSolver.Tours[0].setPrefix_weight(0);
				TspSolver.calc_bound(0); /* Sets lower_bound */

				/* Initialize the priority queue structures */
				TspSolver.PrioQ[1].index = 0;
				TspSolver.PrioQ[1].priority = TspSolver.Tours[0].lower_bound();
				TspSolver.PrioQLast = 1;

				/* Put all unused tours in the free tour stack */
				for (int i = MAX_NUM_TOURS - 1; i > 0; i--)
					TspSolver.TourStack[++TspSolver.TourStackTop] = i;
			}
		});

		/* XXX This just maps threads to intervals. Uncool. */
		Intervals.inline(new VoidInlineTask() {				
			@Override public String toString() { return "Tsp.run"; }
			@Override public void run(Interval subinterval) {
				for(int i = 0; i < nWorkers; i++) {
					new TspSolver(subinterval, "Worker "+i);
				}
			}				
		});

		long end = new Date().getTime();

		System.out.println("tsp-routes_computed\t" + routesComputed);
		System.out.println("tsp-" + nWorkers + "\t"
				+ ((int) end - (int) start));
		System.out.println("Minimum tour length: " + TspSolver.MinTourLen);
		System.out.print("Minimum tour:");
		TspSolver.MakeTourString(TspSize, TspSolver.MinTour);
	}
}
