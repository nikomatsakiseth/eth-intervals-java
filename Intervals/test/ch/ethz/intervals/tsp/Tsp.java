package ch.ethz.intervals.tsp;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.concurrent.atomic.AtomicReference;

import ch.ethz.intervals.AsyncInterval;
import ch.ethz.intervals.InlineInterval;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Constructor;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.ResultTask;

public class Tsp {
	
	Config loadConfig(Interval search, String fname) throws IOException
	{
		BufferedReader in = new BufferedReader(new FileReader(fname));
		int tspSize = Integer.parseInt(in.readLine());
		@Constructor("method") Config config = new Config(search, tspSize);

		for (int i = 0; i < tspSize; i++) {
			String line = in.readLine();
			StringTokenizer tok = new StringTokenizer(line, " ");
			for(int j = 0; tok.hasMoreTokens(); j++)
				config.weights[i][j] = Integer.parseInt(tok.nextToken());
		}
		return config;
	}

	public Result solve(final String fname) throws IOException 
	{
		final Config config[] = new Config[1];
		
		Result result = Intervals.inline(new ResultTask<Result>("root") {
			@Override public Result compute(Interval root) {
				InlineInterval search = Intervals.context().unexecutedInline(
					new AbstractTask("search") {
						@Override
						public void run(Interval search) throws Exception {
							TourElement first = new TourElement(config[0].startNode);
							config[0].enqueue(first);
							search.newAsyncChild(new TspSolver(config[0]));
						}
					}					
				);
				try {
					config[0] = loadConfig(search, fname);
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
				search.execute();
				return config[0].result();
			}
		});
		
		// Sanity checks:
		System.err.printf("minTourLength: %d\n", result.minTourLength);
		
		int calculatedLength = 0;
		boolean seenNodes[] = new boolean[config[0].numNodes];
		check(result.minTour[0] == 0);
		check(config[0].numNodes + 1 == result.minTour.length);
		for(int i = 1; i < result.minTour.length; i++) {
			int currentNode = result.minTour[i];
			int previousNode = result.minTour[i-1];
			int weight = config[0].weights[previousNode][currentNode];
			check(weight > 0);
			calculatedLength += weight; 
			check(!seenNodes[currentNode]);
			seenNodes[currentNode] = true;
		}
		check(calculatedLength == result.minTourLength);
		
		for(int i = 0; i < config[0].numNodes; i++) {
			check(seenNodes[i]);
		}

		System.err.printf("calculatedLength: %d\n", calculatedLength);

		return result;
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
