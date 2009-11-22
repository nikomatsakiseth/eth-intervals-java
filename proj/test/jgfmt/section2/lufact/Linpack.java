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
 *            See below for the previous history of this code              *
 *                                                                         *
 *      This version copyright (c) The University of Edinburgh, 2001.      *
 *                         All rights reserved.                            *
 *                                                                         *
 **************************************************************************/

/*

 Modified 3/3/97 by David M. Doolin (dmd) doolin@cs.utk.edu
 Fixed error in matgen() method. Added some comments.

 Modified 1/22/97 by Paul McMahan mcmahan@cs.utk.edu
 Added more MacOS options to form.

 Optimized by Jonathan Hardwick (jch@cs.cmu.edu), 3/28/96
 Compare to Linkpack.java.
 Optimizations performed:
 - added "final" modifier to performance-critical methods.
 - changed lines of the form "a[i] = a[i] + x" to "a[i] += x".
 - minimized array references using common subexpression elimination.
 - eliminated unused variables.
 - undid an unrolled loop.
 - added temporary 1D arrays to hold frequently-used columns of 2D arrays.
 - wrote my own abs() method
 See http://www.cs.cmu.edu/~jch/java/linpack.html for more details.


 Ported to Java by Reed Wade  (wade@cs.utk.edu) 2/96
 built using JDK 1.0 on solaris
 using "javac -O Linpack.java"


 Translated to C by Bonnie Toy 5/88
 (modified on 2/25/94  to fix a problem with daxpy  for
 unequal increments or equal increments not equal to 1.
 Jack Dongarra)

 */

package jgfmt.section2.lufact;

import jgfmt.jgfutil.*;

public class Linpack {

	double a[][];
	double b[];
	double x[];
	double ops, total, norma, normx;
	double resid, time;
	double kf;
	int n, i, ntimes, info, lda, ldaa, kflops;
	int ipvt[];

	final double abs(double d) {
		return (d >= 0) ? d : -d;
	}

	final double matgen(double a[][], int lda, int n, double b[]) {
		double norma;
		int init, i, j;

		init = 1325;
		norma = 0.0;
		/*
		 * Next two for() statements switched. Solver wants matrix in column
		 * order. --dmd 3/3/97
		 */
		for (i = 0; i < n; i++) {
			for (j = 0; j < n; j++) {
				init = 3125 * init % 65536;
				a[j][i] = (init - 32768.0) / 16384.0;
				norma = (a[j][i] > norma) ? a[j][i] : norma;
			}
		}
		for (i = 0; i < n; i++) {
			b[i] = 0.0;
		}
		for (j = 0; j < n; j++) {
			for (i = 0; i < n; i++) {
				b[i] += a[j][i];
			}
		}

		return norma;
	}

	/*
	 * dgesl solves the double precision system a x = b or trans(a) x = b using
	 * the factors computed by dgeco or dgefa.
	 * 
	 * on entry
	 * 
	 * a double precision[n][lda] the output from dgeco or dgefa.
	 * 
	 * lda integer the leading dimension of the array a .
	 * 
	 * n integer the order of the matrix a .
	 * 
	 * ipvt integer[n] the pivot vector from dgeco or dgefa.
	 * 
	 * b double precision[n] the right hand side vector.
	 * 
	 * job integer = 0 to solve ax = b , = nonzero to solve trans(a)x = b where
	 * trans(a) is the transpose.
	 * 
	 * on return
	 * 
	 * b the solution vector x .
	 * 
	 * error condition
	 * 
	 * a division by zero will occur if the input factor contains a zero on the
	 * diagonal. technically this indicates singularity but it is often caused
	 * by improper arguments or improper setting of lda . it will not occur if
	 * the subroutines are called correctly and if dgeco has set rcond .gt. 0.0
	 * or dgefa has set info .eq. 0 .
	 * 
	 * to compute inverse(a) c where c is a matrix with p columns
	 * dgeco(a,lda,n,ipvt,rcond,z) if (!rcond is too small){ for (j=0,j<p,j++)
	 * dgesl(a,lda,n,ipvt,c[j][0],0); }
	 * 
	 * linpack. this version dated 08/14/78 . cleve moler, university of new
	 * mexico, argonne national lab.
	 * 
	 * functions
	 * 
	 * blas daxpy,ddot
	 */
	final void dgesl(
			double a[][],
			int lda,
			int n,
			int ipvt[],
			double b[],
			int job)
	{
		double t;
		int k, kb, l, nm1, kp1;

		nm1 = n - 1;
		if (job == 0) {

			// job = 0 , solve a * x = b. first solve l*y = b

			if (nm1 >= 1) {
				for (k = 0; k < nm1; k++) {
					l = ipvt[k];
					t = b[l];
					if (l != k) {
						b[l] = b[k];
						b[k] = t;
					}
					kp1 = k + 1;
					daxpy(n - (kp1), t, a[k], kp1, 1, b, kp1, 1);
				}
			}

			// now solve u*x = y

			for (kb = 0; kb < n; kb++) {
				k = n - (kb + 1);
				b[k] /= a[k][k];
				t = -b[k];
				daxpy(k, t, a[k], 0, 1, b, 0, 1);
			}
		} else {

			// job = nonzero, solve trans(a) * x = b. first solve trans(u)*y = b

			for (k = 0; k < n; k++) {
				t = ddot(k, a[k], 0, 1, b, 0, 1);
				b[k] = (b[k] - t) / a[k][k];
			}

			// now solve trans(l)*x = y

			if (nm1 >= 1) {
				for (kb = 1; kb < nm1; kb++) {
					k = n - (kb + 1);
					kp1 = k + 1;
					b[k] += ddot(n - (kp1), a[k], kp1, 1, b, kp1, 1);
					l = ipvt[k];
					if (l != k) {
						t = b[l];
						b[l] = b[k];
						b[k] = t;
					}
				}
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

	/*
	 * forms the dot product of two vectors. jack dongarra, linpack, 3/11/78.
	 */
	final double ddot(
			int n,
			double dx[],
			int dx_off,
			int incx,
			double dy[],
			int dy_off,
			int incy)
	{
		double dtemp;
		int i, ix, iy;

		dtemp = 0;

		if (n > 0) {

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
					dtemp += dx[ix + dx_off] * dy[iy + dy_off];
					ix += incx;
					iy += incy;
				}
			} else {

				// code for both increments equal to 1

				for (i = 0; i < n; i++)
					dtemp += dx[i + dx_off] * dy[i + dy_off];
			}
		}
		return (dtemp);
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
	 * estimate unit roundoff in quantities of size x.
	 * 
	 * this program should function properly on all systems satisfying the
	 * following two assumptions, 1. the base used in representing dfloating
	 * point numbers is not a power of three. 2. the quantity a in statement 10
	 * is represented to the accuracy used in dfloating point variables that are
	 * stored in memory. the statement number 10 and the go to 10 are intended
	 * to force optimizing compilers to generate code satisfying assumption 2.
	 * under these assumptions, it should be true that, a is not exactly equal
	 * to four-thirds, b has a zero for its last bit or digit, c is not exactly
	 * equal to one, eps measures the separation of 1.0 from the next larger
	 * dfloating point number. the developers of eispack would appreciate being
	 * informed about any systems where these assumptions do not hold.
	 * 
	 * 
	 * this routine is one of the auxiliary routines used by eispack iii to
	 * avoid machine dependencies.
	 * 
	 * 
	 * this version dated 4/6/83.
	 */
	final double epslon(double x) {
		double a, b, c, eps;

		a = 4.0e0 / 3.0e0;
		eps = 0;
		while (eps == 0) {
			b = a - 1.0;
			c = b + b + b;
			eps = abs(c - 1.0);
		}
		return (eps * abs(x));
	}

	/*
	 * purpose: multiply matrix m times vector x and add the result to vector y.
	 * 
	 * parameters:
	 * 
	 * n1 integer, number of elements in vector y, and number of rows in matrix
	 * m
	 * 
	 * y double [n1], vector of length n1 to which is added the product mx
	 * 
	 * n2 integer, number of elements in vector x, and number of columns in
	 * matrix m
	 * 
	 * ldm integer, leading dimension of array m
	 * 
	 * x double [n2], vector of length n2
	 * 
	 * m double [ldm][n2], matrix of n1 rows and n2 columns
	 */
	final void dmxpy(
			int n1,
			double y[],
			int n2,
			int ldm,
			double x[],
			double m[][])
	{
		int j, i;

		// cleanup odd vector
		for (j = 0; j < n2; j++) {
			for (i = 0; i < n1; i++) {
				y[i] += x[j] * m[j][i];
			}
		}
	}

}
