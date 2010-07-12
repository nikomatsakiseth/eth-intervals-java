package ch.ethz.intervals.impl;

import java.io.IOException;
import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

import erco.intervals.tsp.Tsp;

public class TestErco {
	@Test public void TspMap16z() throws IOException {		
		Tsp tsp = new Tsp();
		Tsp.Result tour = tsp.solve("erco_test/benchmarks/tsp/tspfiles/map16z");
		Assert.assertEquals(49, tour.minTourLength);
		
		int[][] expected = new int[][] {
			{ 0, 12, 10, 15, 7, 6, 4, 5, 1, 13, 14, 3, 2, 8, 11, 9, 0 },
			{ 0, 2, 8, 7, 6, 4, 5, 1, 13, 14, 3, 12, 10, 15, 9, 11, 0 }
		};
		
		check: {
			for(int i = 0; i < expected.length; i++) {
				if(Arrays.equals(expected[i], tour.minTour)) {
					break check;
				}
			}
			Assert.fail("Did not find an expected path");
		}
	}	
}
