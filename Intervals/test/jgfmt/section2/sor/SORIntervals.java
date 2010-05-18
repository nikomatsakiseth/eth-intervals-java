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
 *      adapted from SciMark 2.0, author Roldan Pozo (pozo@cam.nist.gov)   *
 *                                                                         *
 *      This version copyright (c) The University of Edinburgh, 2001.      *
 *                         All rights reserved.                            *
 *                                                                         *
 **************************************************************************/

package jgfmt.section2.sor;

import jgfmt.jgfutil.JGFInstrumentor;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.task.AbstractTask;

//class RowRecord {
//	private final static boolean DEBUG = false;
//	
//	private final List<Integer> rowRecord = new ArrayList<Integer>();
//	final int M, num_iterations;
//	
//	public RowRecord(int m, int num_iterations) {
//		M = m;
//		this.num_iterations = num_iterations;
//	}
//
//	void addRow(int i) {
//		if(DEBUG) {
//			synchronized(this) {
//				System.err.println("i="+i);
//				rowRecord.add(i);
//			}
//		}
//		//dump(false);
//	}
//	
//	void dump(boolean warn) {
//		if(!DEBUG)
//			return;
//		
//		class Helper {
//			int[] phase;
//			boolean[] found;
//			
//			void printPhase(int p) {
//				StringBuilder sb = new StringBuilder();
//				int[] newPhase = new int[M];
//				for (int j = 0; j < M; j++) {									
//					if (!found[j]) {
//						sb.append(" ");
//						newPhase[j] = phase[j];
//					} else {
//						if (phase[j] >= p 
//								|| j > 1 && phase[j-2] >= p
//								|| j < M-2 && phase[j+2] >= p)
//						{
//							throw new AssertionError(String.format(
//									"Invalid: j=%d p=%d phase[...] = %d,%d,%d", 
//									j, p, (j > 1 ? phase[j-1] : 0), phase[j], (j < M-1 ? phase[j+1] : 0)));
//						}
//						newPhase[j] = phase[j] + 1;
//						sb.append("X");
//					}
//				}
//				phase = newPhase;
//				found = new boolean[M];							
//			}
//
//		}
//		Helper helper = new Helper();
//		
//		int phaseNumber = 1;
//		helper.phase = new int[M];
//		helper.found = new boolean[M];
//		for (Integer i : rowRecord) {
//			if (helper.found[i])
//				helper.printPhase(phaseNumber++);
//			helper.found[i] = true;
//		}
//		
//		helper.printPhase(phaseNumber++);
//		
//		if(warn)
//			for (int j = 1; j < M - 1; j++) {
//				if (helper.phase[j] != num_iterations)
//					System.err.println(String.format(
//							"Row %d processed %d times not %d", j, helper.phase[j], num_iterations));
//			}
//
//	}
//	
//}

public class SORIntervals {

	public static final void SORrun(
			final double omega,
			final double G[][],
			final int num_iterations)
	{
		final int M = G.length;
		final int N = G[0].length;

		final double omega_over_four = omega * 0.25;
		final double one_minus_omega = 1.0 - omega;

		// update interior points
		//
		final int Mm1 = M - 1;
		final int Nm1 = N - 1;
//		final RowRecord rowRecord = new RowRecord(M, num_iterations);
				
		JGFInstrumentor.startTimer("Section2:SOR:Kernel");
		
		Intervals.inline(new AbstractTask() {			
			@Override public void run(Interval subinterval) {
				// Intervals from previous iteration:
				//   (note that null just means "no wait")
				final Interval[][] intervals = new Interval[2][M/2+2];

				// Schedule the various iterations:
				for (int p = 0; p < 2 * num_iterations; p++) {
//					System.err.println("Creating phase " + p + " / " + 2*num_iterations);
					
					int bit = p % 2;
					int prevBit = (bit + 1) % 2;
					
					int i = 1 + (p % 2);
					Interval inter = subinterval.newAsyncChild(new Row(i));
					intervals[bit][1] = inter;
					if(intervals[prevBit][1] != null)
						Intervals.addHbIfNotNull(intervals[prevBit][1], inter);
					if(intervals[prevBit][2] != null)
						Intervals.addHbIfNotNull(intervals[prevBit][2], inter);
					
					i += 2;
					for(int j = 2; i < M; i += 2, j++) {
						inter = subinterval.newAsyncChild(new Row(i));
						intervals[bit][j] = inter;
						Intervals.addHbIfNotNull(intervals[prevBit][j-2], inter);
						Intervals.addHbIfNotNull(intervals[prevBit][j-1], inter);
						Intervals.addHbIfNotNull(intervals[prevBit][j-0], inter);
						Intervals.addHbIfNotNull(intervals[prevBit][j+1], inter);
					}
				}
				
				//System.err.println("Created all intervals");
			}	
			
			class Row extends AbstractTask {
				
				final int i;

				public Row(int i) {
					super("row["+i+"]");
					this.i = i;
				}

				@Override public void run(Interval _) {
					double[] Gi = G[i];
					double[] Gim1 = G[i - 1];
					
//					rowRecord.addRow(i);
					
					if (i == 1) {
						double[] Gip1 = G[i + 1];

						for (int j = 1; j < Nm1; j = j + 2) {
							Gi[j] = omega_over_four
									* (Gim1[j] + Gip1[j] + Gi[j - 1] + Gi[j + 1])
									+ one_minus_omega * Gi[j];
						}
												
					} else if (i == Mm1) {

						double[] Gim2 = G[i - 2];

						for (int j = 1; j < Nm1; j = j + 2) {
							if ((j + 1) != Nm1) {
								Gim1[j + 1] = omega_over_four
										* (Gim2[j + 1] + Gi[j + 1] + Gim1[j] + Gim1[j + 2])
										+ one_minus_omega * Gim1[j + 1];
							}
						}

					} else {

						double[] Gip1 = G[i + 1];
						double[] Gim2 = G[i - 2];

						for (int j = 1; j < Nm1; j = j + 2) {
							Gi[j] = omega_over_four
									* (Gim1[j] + Gip1[j] + Gi[j - 1] + Gi[j + 1])
									+ one_minus_omega * Gi[j];

							if ((j + 1) != Nm1) {
								Gim1[j + 1] = omega_over_four
										* (Gim2[j + 1] + Gi[j + 1] + Gim1[j] + Gim1[j + 2])
										+ one_minus_omega * Gim1[j + 1];
							}
						}
					}				
				}
				
			}
			
		});

		JGFInstrumentor.stopTimer("Section2:SOR:Kernel");
		
//		rowRecord.dump(true);

		for (int i = 1; i < Nm1; i++) {
			for (int j = 1; j < Nm1; j++) {
				SOR.Gtotal += G[i][j];
			}
		}
		
	}

}
