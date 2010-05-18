package erco.intervals.sor;
/*
 * Copyright (C) 2000 by ETHZ/INF/CS
 * All rights reserved
 * 
 * @version $Id: Sor.java 2094 2003-01-30 09:41:18Z praun $
 * @author Florian Schneider
 */

import java.util.Date;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.IndexedTask;

public class Sor {

	public final static int N = 500;
	public final static int M = 500;
	public static int iterations = 100;

	public static final float[][] black_ = new float[M + 2][N + 1];
	public static final float[][] red_ = new float[M + 2][N + 1];

	abstract static class RowIntervalBase extends IndexedTask {

		final int roundsRemaining;

		public RowIntervalBase(int rr) {
			super(1, M+1);
			roundsRemaining = rr;
		}
		
		final void moveUnoffsetRow(float[][] to, float[][] from, int row) {
			for (int col = 0; col < N; col++) {
				to[row][col] = (from[row - 1][col] + from[row + 1][col]
						+ from[row][col] + from[row][col + 1])
						/ (float) 4.0;
			}
		}
		
		final void moveOffsetRow(float[][] to, float[][] from, int row) {
			for (int col = 1; col <= N; col++) {
				to[row][col] = (from[row - 1][col] + from[row + 1][col]
						+ from[row][col] + from[row][col - 1])
						/ (float) 4.0;
			}
		}
		
		public String toString() {
			return getClass().getSimpleName()+"["+roundsRemaining+"]";
		}
				
	}
	
	static class BlackFromRed extends RowIntervalBase {
		
		public BlackFromRed(int rr) {
			super(rr);
		}
		
		@Override
		public void run(Interval current) {
			super.run(current);
			
			Interval inter = current.getParent().newAsyncChild(
					new RedFromBlack(roundsRemaining)
			);
			current.getEnd().addHb(inter.getStart());
		}

		@Override
		public void run(Interval _, int fromRow, int toRow) {
			int row = fromRow;
			
			if((row % 2) == 1) // first row (fromRow) odd
				moveUnoffsetRow(black_, red_, row++);
			
			for(; row < toRow - 1; row += 2) {
				moveOffsetRow(black_, red_, row);       // even row
				moveUnoffsetRow(black_, red_, row + 1); // odd row
			}
			
			if((toRow % 2) == 1) // final row (toRow-1) even
				moveOffsetRow(black_, red_, toRow - 1);
		}
		
	}

	static class RedFromBlack extends RowIntervalBase {
		
		public RedFromBlack(int rr) {
			super(rr);
		}

		@Override
		public void run(Interval current) {
			super.run(current);
			
			if(roundsRemaining > 0) {
				Interval inter = current.getParent().newAsyncChild(
						new BlackFromRed(roundsRemaining - 1)
				);
				current.getEnd().addHb(inter.getStart());
			}
		}

		@Override
		public void run(Interval _, int fromRow, int toRow) {
			int row = fromRow;
			
			if((row % 2) == 1) // first row (fromRow) odd
				moveOffsetRow(red_, black_, row++);
			
			for(; row < toRow - 1; row += 2) {
				moveUnoffsetRow(red_, black_, row);   // even row
				moveOffsetRow(red_, black_, row + 1); // odd row
			}
			
			if((toRow % 2) == 1) // final row (toRow-1) even
				moveUnoffsetRow(red_, black_, toRow - 1);
		}
		
	}
	
	public static void main(String[] args) {

		boolean nop = false;

		try {
			if (args[0].equals("--nop"))
				nop = true;
			else {
				iterations = Integer.parseInt(args[0]);
			}
		} catch (Exception e) {
			System.out
					.println("usage: java SorIntervals <iterations>");
			System.out.println("    or java Sor --nop");
			System.exit(-1);
		}

		// initialize arrays
		int first_row = 1;
		int last_row = M;

		for (int i = first_row; i <= last_row; i++) {
			/*
			 * Initialize the top edge.
			 */
			if (i == 1)
				for (int j = 0; j <= N; j++)
					red_[0][j] = black_[0][j] = (float) 1.0;
			/*
			 * Initialize the left and right edges.
			 */
			if ((i & 1) != 0) {
				red_[i][0] = (float) 1.0;
				black_[i][N] = (float) 1.0;
			} else {
				black_[i][0] = (float) 1.0;
				red_[i][N] = (float) 1.0;
			}
			/*
			 * Initialize the bottom edge.
			 */
			if (i == M)
				for (int j = 0; j <= N; j++)
					red_[i + 1][j] = black_[i + 1][j] = (float) 1.0;
		}

		// start computation
		System.gc();
		long a = new Date().getTime();
		
		if (!nop) {
			Intervals.inline(new AbstractTask() {
				@Override public void run(Interval subinterval) {
					subinterval.newAsyncChild(
							new BlackFromRed(iterations-1)
					);
				}			
			});
		}
		
		long b = new Date().getTime();

		System.out.println("SorIntervals-" + "\t" + Long.toString(b - a));

		// print out results
		float red_sum = 0, black_sum = 0;
		for (int i = 0; i < M + 2; i++)
			for (int j = 0; j < N + 1; j++) {
				red_sum += red_[i][j];
				black_sum += black_[i][j];
			}
		System.out.println("Exiting. red_sum = " + red_sum + ", black_sum = "
				+ black_sum);
	}

	public static void print(String s) {
		System.out.println(Thread.currentThread().getName() + ":" + s);
	}

}
