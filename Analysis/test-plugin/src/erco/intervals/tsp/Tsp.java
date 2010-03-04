package erco.intervals.tsp;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;

import ch.ethz.intervals.*;
import ch.ethz.intervals.quals.*;

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
				config.weights[i][j] = 0; // XXX Integer.parseInt(tok.nextToken());
		}
		return config;
	}

	public int[] solve(String fname) throws IOException 
	{
		final Config config = loadConfig(fname);
		TourElement first = new TourElement(config.startNode);
		config.enqueue(first);
		Intervals.inline(new RunSolver(config));		
		return config.minTour;
	}
	
	public static void main(String args[]) throws IOException {
		for(int i = 0; i < args.length; i++) {
			String fname = args[i];
			Tsp tsp = new Tsp();
			int[] tour = tsp.solve(fname);
			
			//XXX System.out.printf("%s:", fname);
			//XXX for(int j = 0; i < tour.length; i++)
			//XXX	System.out.printf(" %d", tour[j]);
			//XXX System.out.println();
		}
	}
	
}
