package erco.intervals.tsp;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Constructor;
import ch.ethz.intervals.task.AbstractTask;

public class Tsp {
	
	public static class Result {
		final public int minTourLength;
		final public int minTour[];
		
		public Result(int minTourLength, int[] minTour) {
			this.minTourLength = minTourLength;
			this.minTour = minTour;
		}
	}
	
	Config loadConfig(String fname) throws IOException
	{
		BufferedReader in = new BufferedReader(new FileReader(fname));
		int tspSize = Integer.parseInt(in.readLine());
		@Constructor("method") Config config = new Config(tspSize);

		for (int i = 0; i < tspSize; i++) {
			String line = in.readLine();
			StringTokenizer tok = new StringTokenizer(line, " ");
			for(int j = 0; tok.hasMoreTokens(); j++)
				config.weights[i][j] = Integer.parseInt(tok.nextToken());
		}
		return config;
	}

	public Result solve(String fname) throws IOException 
	{
		final Config config = loadConfig(fname);
		TourElement first = new TourElement(config.startNode);
		config.enqueue(first);
		Intervals.inline(new AbstractTask() {
			@Override public void run(Interval subinterval) {
				subinterval.newAsyncChild(new TspSolver(config));
			}
		});
		
		// Sanity checks:
		System.err.printf("minTourLength: %d\n", config.minTourLength);
		
		int calculatedLength = 0;
		boolean seenNodes[] = new boolean[config.numNodes];
		check(config.minTour[0] == 0);
		check(config.numNodes + 1 == config.minTour.length);
		for(int i = 1; i < config.minTour.length; i++) {
			int currentNode = config.minTour[i];
			int previousNode = config.minTour[i-1];
			int weight = config.weights[previousNode][currentNode];
			check(weight > 0);
			calculatedLength += weight; 
			check(!seenNodes[currentNode]);
			seenNodes[currentNode] = true;
		}
		check(calculatedLength == config.minTourLength);
		
		for(int i = 0; i < config.numNodes; i++) {
			check(seenNodes[i]);
		}

		System.err.printf("calculatedLength: %d\n", calculatedLength);

		return new Result(config.minTourLength, config.minTour);
	}
	
	private void check(boolean b) {
		if(!b) throw new RuntimeException("Check failed");
	}

	public static void main(String args[]) throws IOException {
		for(String fname : args) {
			Tsp tsp = new Tsp();
			int[] tour = tsp.solve(fname).minTour;
			
			System.out.printf("%s:", fname);
			for(int i = 0; i < tour.length; i++)
				System.out.printf(" %d", tour[i]);
			System.out.println();
		}
	}
	
}
