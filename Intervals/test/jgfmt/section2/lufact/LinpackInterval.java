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

package jgfmt.section2.lufact;

import ch.ethz.intervals.IndexedInterval;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.VoidSubinterval;

class LinpackInterval {

	int lda, n, ipvt[];
	double a[][];

	public LinpackInterval(
			double a[][],
			int lda,
			int n,
			int ipvt[])
	{
		this.a = a;
		this.lda = lda;
		this.n = n;
		this.ipvt = ipvt;
	}

	final double abs(double d) {
		return (d >= 0) ? d : -d;
	}

	/*
	 * dgefa factors a double precision matrix by gaussian elimination.
	 * 
	 * dgefa is usually called by dgeco, but it can be called directly with a
	 * saving in time if rcond is not needed. (time for dgeco) = (1 + 9/n)(time
	 * for dgefa) .
	 * 
	 * on entry
	 * 
	 * a double precision[n][lda] the matrix to be factored.
	 * 
	 * lda integer the leading dimension of the array a .
	 * 
	 * n integer the order of the matrix a .
	 * 
	 * on return
	 * 
	 * a an upper triangular matrix and the multipliers which were used to
	 * obtain it. the factorization can be written a = lu where l is a product
	 * of permutation and unit lower triangular matrices and u is upper
	 * triangular.
	 * 
	 * ipvt integer[n] an integer vector of pivot indices.
	 * 
	 * info integer = 0 normal value. = k if u[k][k] .eq. 0.0 . this is not an
	 * error condition for this subroutine, but it does indicate that dgesl or
	 * dgedi will divide by zero if called. use rcond in dgeco for a reliable
	 * indication of singularity. linpack. this version dated 08/14/78. cleve
	 * moler, university of new mexico, argonne national lab.
	 * 
	 * functions
	 * 
	 * blas daxpy,dscal,idamax
	 */

	public void run() {
		// gaussian elimination with partial pivoting

		final int nm1 = n - 1;

		if (nm1 >= 0) {
			for (int k = 0; k < nm1; k++) {
				final double[] col_k = a[k];
				final int kp1 = k + 1;

				// find l = pivot index

				final int l = idamax(n - k, col_k, k, 1) + k;
				ipvt[k] = l;

				// zero pivot implies this column already triangularized

				if (col_k[l] != 0) {
					// interchange if necessary
					if (l != k) {
						double t = col_k[l];
						col_k[l] = col_k[k];
						col_k[k] = t;
					}

					// compute multipliers
					double t = -1.0 / col_k[k];
					dscal(n - (kp1), t, col_k, kp1, 1);

					// row elimination with column indexing
					final int k0 = k;
					Intervals.blockingInterval(new VoidSubinterval() {						
						@Override public void run(Interval subinterval) {
							new IndexedInterval(subinterval, kp1, n) {						
								@Override
								public void run(Point parentEnd, int fromIndex, int toIndex) {
									for(int j = fromIndex; j < toIndex; j++) {
										double[] col_j = a[j];
										double t = col_j[l];
			
										if (l != k0) {
											col_j[l] = col_j[k0];
											col_j[k0] = t;
										}
			
										daxpy(n - (kp1), t, col_k, kp1, 1, col_j, kp1, 1);
									}
								}
							};
						}
					});
				} 
			}
		}

		ipvt[n - 1] = n - 1;

	}

	/*
	 * finds the index of element having max. absolute value. jack dongarra,
	 * linpack, 3/11/78.
	 */
	final int idamax(int n, double dx[], int dx_off, int incx) {
		double dmax, dtemp;
		int i, ix, itemp = 0;

		if (n < 1) {
			itemp = -1;
		} else if (n == 1) {
			itemp = 0;
		} else if (incx != 1) {

			// code for increment not equal to 1

			dmax = abs(dx[0 + dx_off]);
			ix = 1 + incx;
			for (i = 1; i < n; i++) {
				dtemp = abs(dx[ix + dx_off]);
				if (dtemp > dmax) {
					itemp = i;
					dmax = dtemp;
				}
				ix += incx;
			}
		} else {

			// code for increment equal to 1

			itemp = 0;
			dmax = abs(dx[0 + dx_off]);
			for (i = 1; i < n; i++) {
				dtemp = abs(dx[i + dx_off]);
				if (dtemp > dmax) {
					itemp = i;
					dmax = dtemp;
				}
			}
		}
		return (itemp);
	}

	/*
	 * scales a vector by a constant. jack dongarra, linpack, 3/11/78.
	 */
	final void dscal(int n, double da, double dx[], int dx_off, int incx) {
		int i, nincx;

		if (n > 0) {
			if (incx != 1) {

				// code for increment not equal to 1

				nincx = n * incx;
				for (i = 0; i < nincx; i += incx)
					dx[i + dx_off] *= da;
			} else {

				// code for increment equal to 1

				for (i = 0; i < n; i++)
					dx[i + dx_off] *= da;
			}
		}
	}

	/*
	 * constant times a vector plus a vector. jack dongarra, linpack, 3/11/78.
	 */
	final void daxpy(
			int n,
			double da,
			double dx[],
			int dx_off,
			int incx,
			double dy[],
			int dy_off,
			int incy)
	{
		int i, ix, iy;

		if ((n > 0) && (da != 0)) {
			if (incx != 1 || incy != 1) {

				// code for unequal increments or equal increments not equal to
				// 1

				ix = 0;
				iy = 0;
				if (incx < 0)
					ix = (-n + 1) * incx;
				if (incy < 0)
					iy = (-n + 1) * incy;
				for (i = 0; i < n; i++) {
					dy[iy + dy_off] += da * dx[ix + dx_off];
					ix += incx;
					iy += incy;
				}
				return;
			} else {

				// code for both increments equal to 1

				for (i = 0; i < n; i++)
					dy[i + dy_off] += da * dx[i + dx_off];
			}
		}
	}

}
