package ch.ethz.intervals;

import java.util.Random;

import junit.framework.Assert;

import org.junit.Test;

/**
 * Based on code from the Intel Threading Challenge.
 */
public class TestStrassenMMult {
	
	static final int GRAIN = 1024;
	
	class Matrix {
		private final double[][] data;
		private final int rowOffset, colOffset;
		
		public Matrix(double[][] data, int rowOffset, int colOffset) {
			this.data = data;
			this.rowOffset = rowOffset;
			this.colOffset = colOffset;
		}
		
		public Matrix(int rows, int cols) {
			data = new double[rows][cols];
			rowOffset = 0;
			colOffset = 0;
		}

		public double get(int row, int col) {
			return data[row+rowOffset][col+colOffset];
		}
		
		public void set(int row, int col, double value) {
			data[row+rowOffset][col+colOffset] = value;
		}
		
		public void add(int row, int col, double value) {
			data[row+rowOffset][col+colOffset] += value;
		}
		
		public Matrix submatrix(int ro, int co) {
			return new Matrix(data, rowOffset + ro, colOffset + co);
		}		
	}
	
	Matrix sequentialMatrixMultiply(int m, int n, int p, Matrix a, Matrix b) {
		Matrix c = new Matrix(m, n);
		for(int i = 0; i < m; i++)
			for(int j = 0; j < n; j++) {
				c.set(i, j, 0);
				for(int k = 0; k < p; k++)
					c.add(i, j, a.get(i, k) * b.get(k, j));
			}
		return c;
	}
	
	// mf, ml; /* first and last+1 i index */  
	// nf, nl; /* first and last+1 j index */  
	// pf, pl; /* first and last+1 k index */  
	void matmultleaf(int mf, int ml, int nf, int nl, int pf, int pl, Matrix A, Matrix B, Matrix C) {
		for (int i = mf; i < ml; i++)   
			for (int j = nf; j < nl; j++)   
				for (int k = pf; k < pl; k++)
					C.add(i, j, A.get(i, k) * B.get(k, j));
	}
	
	void addMatrixBlocks(Matrix t, int m, int n, Matrix x, Matrix y) {
		for(int i = 0; i < m; i++)
			for(int j = 0; j < n; j++)
				t.set(i, j, x.get(i, j) + y.get(i, j));
	}
	
	void subMatrixBlocks(Matrix t, int m, int n, Matrix x, Matrix y) {
		for(int i = 0; i < m; i++)
			for(int j = 0; j < n; j++)
				t.set(i, j, x.get(i, j) - y.get(i, j));		
	}
	
	void strassenMult(
			final int mf, final int ml, 
			final int nf, final int nl, 
			final int pf, final int pl,
			final Matrix A,
			final Matrix B,
			final Matrix C)
	{
		if((ml-mf) * (nl-nf) * (pl - pf) < GRAIN) {
			matmultleaf(mf, ml, nf, nl, pf, pl, A, B, C);
		} else {
			final int m2 = (ml - mf) / 2;
			final int n2 = (nl - nf) / 2;
			final int p2 = (pl - pf) / 2;
			
			final Matrix M1 = new Matrix(m2, n2);
			final Matrix M2 = new Matrix(m2, n2);
			final Matrix M3 = new Matrix(m2, n2);
			final Matrix M4 = new Matrix(m2, n2);
			final Matrix M5 = new Matrix(m2, n2);
			final Matrix M6 = new Matrix(m2, n2);
			final Matrix M7 = new Matrix(m2, n2);
			
			final Matrix A11 = A.submatrix(mf, pf); 
			final Matrix A12 = A.submatrix(mf, p2); 
			final Matrix A21 = A.submatrix(m2, pf); 
			final Matrix A22 = A.submatrix(m2, p2); 

			final Matrix B11 = B.submatrix(pf, nf); 
			final Matrix B12 = B.submatrix(pf, n2); 
			final Matrix B21 = B.submatrix(p2, nf); 
			final Matrix B22 = B.submatrix(p2, n2); 
			
			final Matrix C11 = C.submatrix(mf, nf); 
			final Matrix C12 = C.submatrix(mf, n2); 
			final Matrix C21 = C.submatrix(m2, nf); 
			final Matrix C22 = C.submatrix(m2, n2);
			
			Intervals.blockingInterval(new VoidSubinterval() {				
				@Override public void run(Interval subinterval) {
					
					// M1 = (A11 + A22)*(B11 + B22)
					new Interval(Intervals.child()) {
						@Override public void run() {
							Matrix tAM1 = new Matrix(m2, p2);
							addMatrixBlocks(tAM1, m2, p2, A11, A22);
							Matrix tBM1 = new Matrix(p2, n2);
							addMatrixBlocks(tBM1, p2, n2, B11, B22);
							strassenMult(0, m2, 0, n2, 0, p2, tAM1, tBM1, M1);
						}						
					};
					
					//M2 = (A21 + A22)*B11
					new Interval(Intervals.child()) {
						@Override public void run() {
							Matrix tAM2 = new Matrix(m2, p2);
							addMatrixBlocks(tAM2, m2, p2, A21, A22);
							strassenMult(0, m2, 0, n2, 0, p2, tAM2, B11, M2);
						}
					};
					
					//M3 = A11*(B12 - B22)
					new Interval(Intervals.child()) {
						@Override public void run() {
							Matrix tBM3 = new Matrix(p2, n2);
							subMatrixBlocks(tBM3, p2, n2, B12, B22);
							strassenMult(0, m2, 0, n2, 0, p2, A11, tBM3, M3);
						}
					};
					
					//M4 = A22*(B21 - B11)
					new Interval(Intervals.child()) {
						@Override public void run() {
							Matrix tBM4 = new Matrix(p2, n2);
							subMatrixBlocks(tBM4, p2, n2, B21, B11);
							strassenMult(0, m2, 0, n2, 0, p2, A22, tBM4, M4);
						}
					};
					
					//M5 = (A11 + A12)*B22
					new Interval(Intervals.child()) {
						@Override public void run() {
							Matrix tAM5 = new Matrix(m2, p2);
							addMatrixBlocks(tAM5, m2, p2, A11, A12);
							strassenMult(0, m2, 0, n2, 0, p2, tAM5, B22, M5);
						}
					};
					
					//M6 = (A21 - A11)*(B11 + B12)
					new Interval(Intervals.child()) {
						@Override public void run() {
							Matrix tAM6 = new Matrix(m2, p2);
							Matrix tBM6 = new Matrix(p2, n2);
							subMatrixBlocks(tAM6, m2, p2, A21, A11);
							addMatrixBlocks(tBM6, p2, n2, B11, B12);
							strassenMult(0, m2, 0, n2, 0, p2, tAM6, tBM6, M6);
						}
					};

					//M7 = (A12 - A22)*(B21 + B22)
					new Interval(Intervals.child()) {
						@Override public void run() {
							Matrix tAM7 = new Matrix(m2, p2);
							Matrix tBM7 = new Matrix(p2, n2);
							subMatrixBlocks(tAM7, m2, p2, A12, A22);
							addMatrixBlocks(tBM7, p2, n2, B21, B22);
							strassenMult(0, m2, 0, n2, 0, p2, tAM7, tBM7, M7);
						}
					};
				}
			});

			for (int i = 0; i < m2; i++)
				for (int j = 0; j < n2; j++) {
					C11.set(i, j, M1.get(i,j) + M4.get(i,j) - M5.get(i,j) + M7.get(i,j));
					C12.set(i, j, M3.get(i,j) + M5.get(i,j));
					C21.set(i, j, M2.get(i,j) + M4.get(i,j));
					C22.set(i, j, M1.get(i,j) - M2.get(i,j) + M3.get(i,j) + M6.get(i,j));
				}

		}
	}
	
	Matrix matmultS(int m, int n, int p, Matrix A, Matrix B) {
		Matrix C = new Matrix(m, n);
		strassenMult(0, m, 0, n, 0, p, A, B, C);
		return C;
	}
	
	Matrix randomMatrix(Random r, int m, int n) {
		Matrix A = new Matrix(m, n);
		for(int i = 0; i < m; i++)
			for(int j = 0; j < n; j++)
				A.set(i, j, 5.0 - r.nextDouble() * 10);
		return A;
		
	}
	
	@Test
	public void testRandomMatrix() {
		Random r = new Random();
		
		final int m = 512;
		final int n = 512;
		final int p = 512;
		
		Matrix A = randomMatrix(r, m, p);
		Matrix B = randomMatrix(r, p, n);
		long t0 = System.nanoTime();
		Matrix C = sequentialMatrixMultiply(m, n, p, A, B);
		long t1 = System.nanoTime();
		Matrix C4 = matmultS(m, n, p, A, B);
		long t2 = System.nanoTime();

		final double e = 1.0e-3; // ignore rounding errors
		for(int i = 0; i < m; i++) 
			for(int j = 0; j < n; j++)
				Assert.assertTrue(
						String.format("(%d, %d) differ", i, j),
						Math.abs(C.get(i, j) - C4.get(i, j)) < e);
		
		System.err.printf("Parallel/Sequential time ratio: %.0f%%\n",
				(100.0 * (t2 - t1)) / (t1 - t0));
	}
}
