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
				config.weights[i][j] = Integer.parseInt(tok.nextToken());
		}
		return config;
	}

	public int/*@Creator("hbNow")*/[] solve(String fname) throws IOException 
	{
		final Config config = loadConfig(fname);
		TourElement first = new TourElement(config.startNode);
		config.enqueue(first);
		Intervals.inline(new RunSolver(config));		
		return config.getBest();
	}
	
	public static void main(String/*@Creator("writableBy method")*/[] args) throws IOException {
		for(int a = 0; a < args.length; a++) {
		    String fname = args[a];
			Tsp tsp = new /*@Creator("method")*/ Tsp();
			int/*@Creator("hbNow")*/[] tour = tsp.solve(fname);
			
			System.out.println(fname+":");
			for(int i = 0; i < tour.length; i++)
				System.out.printf(" %d", tour[i]);
			System.out.println();
		}
	}
	
}
