package erco.intervals.tsp;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.VoidInlineTask;
import ch.ethz.intervals.quals.Constructor;

public class Tsp {
	
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

	public int[] solve(String fname) throws IOException 
	{
		final Config config = loadConfig(fname);
		TourElement first = new TourElement(config.startNode);
		config.enqueue(first);
		Intervals.inline(new VoidInlineTask() {
			@Override public void run(Interval subinterval) {
				new TspSolver(subinterval, config);
			}
		});		
		return config.minTour;
	}
	
	public static void main(String args[]) throws IOException {
		for(String fname : args) {
			Tsp tsp = new Tsp();
			int[] tour = tsp.solve(fname);
			
			System.out.printf("%s:", fname);
			for(int i = 0; i < tour.length; i++)
				System.out.printf(" %d", tour[i]);
			System.out.println();
		}
	}
	
}
