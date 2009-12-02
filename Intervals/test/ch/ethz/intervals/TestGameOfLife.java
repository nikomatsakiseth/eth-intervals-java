package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import junit.framework.Assert;

import org.junit.Test;

public class TestGameOfLife {
	
	/** If set to true, then we dump the configuration at each intermediate
	 *  stage.  Useful for debugging! */
	public static final boolean DUMP_ALL_CONFIGS = false;
	
	/** 
	 * Encodes the basic rules of the game of life. 
	 * @param oldValue is 0 if the cell used to be dead or 1 otherwise
	 * @param total is the total number of live neighbors (not including the cell itself) 
	 * @return 0 if the cell is now dead or 1 if it is now alive */
	public byte gameOfLife(byte oldValue, int total) {
		// The basic rules:
		if(oldValue != 0) {
			if(total >= 2 && total <= 3) return 1;
			return 0;
		}
		
		if(total == 3) return 1;
		return 0;
	}
	
	/** Encodes the basic rules of the game of life.  Assumes 1-based row
	 *  and col indices. */
	public void gameOfLife(byte[][] inData, byte[][] outData, int row, int col) {
		byte current = inData[row][col]; 
		int total = -current;
		for(int r = row - 1; r <= row + 1; r++) {
			total += inData[r][col - 1];
			total += inData[r][col];
			total += inData[r][col + 1];
		}
		
		outData[row][col] = gameOfLife(current, total);
	}
	
	class Tile {
		public final int tr, tc; /** tile row, tile column */
		public final int r0, rN; /** first, last row       */
		public final int c0, cN; /** first, last col       */
		
		public Tile(int tr, int tc, int r0, int rN, int c0, int cN) {
			this.tr = tr;
			this.tc = tc;
			this.r0 = r0;
			this.rN = rN;
			this.c0 = c0;
			this.cN = cN;
		}

		public void run(byte[][] inData, byte[][] outData) {
			for(int ri = r0; ri <= rN; ri++)
				for(int ci = c0; ci <= cN; ci++) 				
					gameOfLife(inData, outData, ri, ci);
		}		
	}
	
	interface InvokableWithTile {
		void run(Tile t);
	}
	
	void subdivide(int rows, int cols, int w, int h, InvokableWithTile run) {
		int tileRows = (rows + h - 1) / h;
		int tileCols = (cols + w - 1) / w;
		
		for(int tr = 1; tr <= tileRows; tr++) {
			int r0 = (tr - 1) * h + 1;
			int rN = Math.min(r0 + h - 1, rows);
			
			for(int tc = 1; tc <= tileCols; tc++) {
				int c0 = (tc - 1) * w + 1;
				int cN = Math.min(c0 + w - 1, cols);
				
				Tile t = new Tile(tr, tc, r0, rN, c0, cN);
				run.run(t);
			}
		}
	}

	/** Simple abstraction layer for testing and comparing different engines */ 
	interface GameOfLifeEngine {
		/** Runs the game of life given the initial configuration {@code config},
		 *  which must be a square matrix.  The routine should execute
		 *  {@code numRounds} gens and return the final configuration. */
		byte[][] execute(byte[][] initialConfiguration, int rs, int cs, int numRounds);
	}
	
	abstract class GameOfLifeBoard {
		
		final int rs, cs;
		final byte[][][] data;
		final int numGens;
		
		GameOfLifeBoard(byte[][] initialConfiguration, int rs, int cs, int numRounds) {
			this.rs = rs;
			this.cs = cs;
			this.numGens = numRounds;			
			this.data = new byte[2][rs+2][cs+2];
			
			for(int r = 1; r <= rs; r++)
				for(int c = 1; c <= cs; c++)
					data[0][r][c] = initialConfiguration[r][c];
		}
		
		abstract byte[][] execute();
		
	}
	
	class SerialBoard extends GameOfLifeBoard {
		
		public SerialBoard(byte[][] initialConfiguration, int rs, int cs, int numGens) {
			super(initialConfiguration, rs, cs, numGens);
		}

		@Override
		byte[][] execute() {
			for(int g = 0; g < numGens; g++) {
				byte[][] inData = data[g % 2], outData = data[(g + 1) % 2];
				for(int r = 1; r <= rs; r++)
					for(int c = 1; c <= cs; c++)
						gameOfLife(inData, outData, r, c);
			}
			return data[numGens % 2];
		}
		
	}
	
	class SerialEngine implements GameOfLifeEngine {

		@Override
		public byte[][] execute(byte[][] initialConfiguration, int rs, int cs, int numGens) {
			SerialBoard sb = new SerialBoard(initialConfiguration, rs, cs, numGens);
			return sb.execute();
		}

	}
	
	class SerialTiledBoard extends GameOfLifeBoard {
		
		final int w, h;
		
		public SerialTiledBoard(byte[][] initialConfiguration, int rs, int cs, int numGens, int w, int h) {
			super(initialConfiguration, rs, cs, numGens);
			this.w = w;
			this.h = h;
		}

		@Override
		byte[][] execute() {
			for(int g = 0; g < numGens; g++) {
				final byte[][] inData = data[g % 2], outData = data[(g + 1) % 2];
				subdivide(rs, cs, w, h, new InvokableWithTile() {					
					@Override
					public void run(Tile t) {
						t.run(inData, outData);
					}
				});
			}
			return data[numGens % 2];
		}
		
	}
	
	class SerialTiledEngine implements GameOfLifeEngine {
		
		public final int w, h;
		
		public SerialTiledEngine() {
			this(10, 10);
		}

		public SerialTiledEngine(int w, int h) {
			this.w = w;
			this.h = h;
		}		

		@Override
		public byte[][] execute(byte[][] initialConfiguration, int rs, int cs, int maxGen) {
			SerialTiledBoard sb = new SerialTiledBoard(initialConfiguration, rs, cs, maxGen, w, h);
			return sb.execute();
		}

	}
	
	class PhasedIntervalBoard extends GameOfLifeBoard {
		
		final int w, h;
		
		int gensRemaining;
		Interval nextGen;
		
		public PhasedIntervalBoard(byte[][] initialConfiguration, int rs, int cs, int numGens, int w, int h) {
			super(initialConfiguration, rs, cs, numGens);
			this.w = w;
			this.h = h;			
		}

		void switchGens(Point endOfThisGen) {
			Interval switchInterval = Intervals.intervalWithBound(endOfThisGen.bound(), switchGenTask);
			Intervals.addHb(endOfThisGen, switchInterval.start());
			
			nextGen = Intervals.intervalWithBound(endOfThisGen.bound(), Intervals.emptyTask);
			Intervals.addHb(switchInterval.end(), nextGen.start());
			
			Intervals.schedule();
		}
		
		class SwitchGenTask extends AbstractTask {
			
			@Override
			public void run(Point currentEnd) {
				if(gensRemaining-- > 0)
					switchGens(nextGen.end());
			}
			
		}
		final SwitchGenTask switchGenTask = new SwitchGenTask();
		
		public void startTile(Tile tile, byte[][] inBoard, byte[][] outBoard) {
			Intervals.intervalDuring(nextGen, new TileTask(tile, inBoard, outBoard));
		}

		class TileTask extends AbstractTask {
			public final Tile tile;
			public final byte[][] inBoard;
			public final byte[][] outBoard;
			
			public TileTask(
					Tile tile,
					byte[][] inBoard,
					byte[][] outBoard) 
			{
				this.tile = tile;
				this.inBoard = inBoard;
				this.outBoard = outBoard;
			}

			public String toString() 
			{
				return "Tile("+tile.tr+","+tile.tc+")";
			}			
			
			@Override
			public void run(Point currentEnd) {
				tile.run(inBoard, outBoard);
				if(gensRemaining > 0)
					startTile(tile, outBoard, inBoard);					
			}
		}

		@Override
		byte[][] execute() {
			gensRemaining = numGens;
			
			Intervals.blockingInterval(new SubintervalTask(new AbstractTask() {
				
				@Override
				public void run(Point currentEnd) {
					switchGens(currentEnd);
					
					subdivide(rs, cs, w, h, new InvokableWithTile() {					
						@Override
						public void run(Tile t) {
							startTile(t, data[0], data[1]);
						}
					});
				}
				
			}));

			return data[numGens % 2];
		}
		
	}
	
	/**
	 * A version that uses phases.  Each gen fully completes before
	 * the next begins.  The structure follows a generic barrier
	 * pattern, where at any given moment there is an interval
	 * {@code nextGen} where processing for the next gen is
	 * scheduled.  You could also write this as a simple for loop
	 * with a bounded interval for each iteration.  
	 */
	class PhasedIntervalEngine implements GameOfLifeEngine {
		
		public final int w, h;
		
		public PhasedIntervalEngine() {
			this(10, 10);
		}

		public PhasedIntervalEngine(int w, int h) {
			this.w = w;
			this.h = h;
		}
		
		@Override
		public byte[][] execute(byte[][] initialConfig, int rs, int cs, int numGens) {
			PhasedIntervalBoard board = new PhasedIntervalBoard(initialConfig, rs, cs, numGens, w, h);
			return board.execute();
		}		
	}
	
	class IndexedIntervalBoard extends GameOfLifeBoard {
		
		public IndexedIntervalBoard(byte[][] initialConfiguration, int rs, int cs, int numGens) {
			super(initialConfiguration, rs, cs, numGens);
		}

		class ColTask extends IndexedTask {
			
			final int row, gen;

			protected ColTask(int gen, int row) {
				super(1, cs+1);
				this.gen = gen;
				this.row = row;
			}

			@Override
			public void run(Point parentEnd, int fromIndex, int toIndex) {
				byte[][] inData = data[gen % 2];
				byte[][] outData = data[(gen + 1) % 2];
				for(int c = fromIndex; c < toIndex; c++)
					gameOfLife(inData, outData, row, c);
			}
			
		}

		class GenTask extends IndexedTask {

			final int gen;

			protected GenTask(int gen) {
				super(1, rs+1);
				this.gen = gen;
			}

			@Override
			public void run(Point parentEnd, int fromIndex, int toIndex) {
				for(int r = fromIndex; r < toIndex; r++) {
					Intervals.childInterval(new ColTask(gen, r));
					Intervals.schedule();
				}
			}
			
		}

		@Override
		byte[][] execute() {
			Intervals.blockingInterval(new AbstractTask() {
				
				@Override
				public void run(Point currentEnd) {
					Point prevGen = null;
					for(int gen = 0; gen < numGens; gen++) {
						Interval thisGen = Intervals.childInterval(new GenTask(gen));
						Intervals.addHb(prevGen, thisGen.start());
						Intervals.schedule();
						prevGen = thisGen.end();
					}
				}
				
			});

			return data[numGens % 2];
		}
		
	}
	
	/**
	 * A version that uses phases.  Each gen fully completes before
	 * the next begins.  The structure follows a generic barrier
	 * pattern, where at any given moment there is an interval
	 * {@code nextGen} where processing for the next gen is
	 * scheduled.  You could also write this as a simple for loop
	 * with a bounded interval for each iteration.  
	 */
	class IndexedIntervalEngine implements GameOfLifeEngine {
		
		public IndexedIntervalEngine() {
		}
		
		@Override
		public byte[][] execute(byte[][] initialConfig, int rs, int cs, int numGens) {
			IndexedIntervalBoard board = new IndexedIntervalBoard(initialConfig, rs, cs, numGens);
			return board.execute();
		}		
	}
	
	final ExecutorService executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
	
	class BarrierBoard extends GameOfLifeBoard {
		
		final CyclicBarrier barrier;
		
		public BarrierBoard(byte[][] initialConfiguration, int rs, int cs, int numGens) {
			super(initialConfiguration, rs, cs, numGens);
			barrier = new CyclicBarrier(Runtime.getRuntime().availableProcessors());
		}
		
		class TileTask implements Callable<Void> {
			public final Tile tile;
			
			public TileTask(Tile tile) 
			{
				this.tile = tile;
			}

			public String toString() 
			{
				return "Tile("+tile.tr+","+tile.tc+")";
			}			

			@Override
			public Void call() throws Exception {
				for(int i = 0; i < numGens; i++) {
					tile.run(data[i%2], data[(i+1)%2]);
					barrier.await();
				}
				return null;
			}
		}

		@Override
		byte[][] execute() {
			int parties = barrier.getParties();
			int w = (cs + parties - 1) / parties;
			int h = rs;
			
			final List<Future<?>> futures = new ArrayList<Future<?>>();
			subdivide(rs, cs, w, h, new InvokableWithTile() {					
				public void run(Tile t) {
					futures.add(executorService.submit(new TileTask(t)));
				}
			});
			
			for(Future<?> f : futures)
				try {
					f.get();
				} catch(Exception e) {}

			return data[numGens % 2];
		}
		
	}
	
	/**
	 * A version that uses phases.  Each gen fully completes before
	 * the next begins.  The structure follows a generic barrier
	 * pattern, where at any given moment there is an interval
	 * {@code nextGen} where processing for the next gen is
	 * scheduled.  You could also write this as a simple for loop
	 * with a bounded interval for each iteration.  
	 */
	class BarrierEngine implements GameOfLifeEngine {
		
		@Override
		public byte[][] execute(byte[][] initialConfig, int rs, int cs, int numGens) {
			BarrierBoard board = new BarrierBoard(initialConfig, rs, cs, numGens);
			return board.execute();
		}		
	}

	class SimplerPhasedIntervalBoard extends GameOfLifeBoard {
		
		final int w, h;
		
		SimplerPhasedIntervalBoard(byte[][] initialConfiguration, int rs,
				int cs, int numGens, int w, int h) 
		{
			super(initialConfiguration, rs, cs, numGens);
			this.w = w;
			this.h = h;
		}

		@Override
		byte[][] execute() {
			for(int gen = 0; gen < numGens; gen++) {
				final byte[][] inBoard = data[gen % 2];
				final byte[][] outBoard = data[(gen + 1) % 2];
				
				Intervals.blockingInterval(new AbstractTask() {
					public void run(final Point currentEnd) {
						subdivide(rs, cs, w, h, new InvokableWithTile() {
							public void run(final Tile t) {
								Intervals.intervalWithBound(currentEnd, new AbstractTask() {
									public String toString() 
									{
										return "Tile("+t.tr+","+t.tc+")";
									}

									public void run(Point currentEnd) {
										t.run(inBoard, outBoard);
									}
								});
								
							}
						});
					}
				});
			}
			return data[numGens % 2];
		}
	}
	
	class SimplerPhasedIntervalEngine implements GameOfLifeEngine {
		
		public final int w, h;
		
		public SimplerPhasedIntervalEngine() {
			this(10, 10);
		}

		public SimplerPhasedIntervalEngine(int w, int h) {
			this.w = w;
			this.h = h;
		}
		
		@Override
		public byte[][] execute(byte[][] initialConfig, int rs, int cs, int numGens) {
			SimplerPhasedIntervalBoard sib = new SimplerPhasedIntervalBoard(initialConfig, rs, cs, numGens, w, h);
			return sib.execute();
		}		
	}
	
	class FineGrainedIntervalBoard extends GameOfLifeBoard {
		
		final int w, h;
		final Point[][][] points;

		FineGrainedIntervalBoard(byte[][] initialConfiguration, int rs, int cs,
				int numGens, int w, int h) {
			super(initialConfiguration, rs, cs, numGens);
			this.w = w;
			this.h = h;
			points = new Point[2][rs+2][cs+2];
		}

		class TileTask implements Task {
			public final Tile tile;
			public final int gen;
			
			public TileTask(Tile tile, int gen) {
				this.tile = tile;
				this.gen = gen;
			}

			public String toString() 
			{
				return "Tile("+gen+","+tile.tr+","+tile.tc+")";
			}

			@Override
			public void addDependencies(Interval inter) {
				if(gen > 0) {
					// n.b.: The points array is padded with nulls.
					Point[][] prevGen = points[(gen - 1) % 2];					
					for(int r = -1; r <= 1; r++)
						for(int c = -1; c <= 1; c++) {
							Intervals.addHb(prevGen[tile.tr+r][tile.tc+c], inter.start());
						}
				}
			}
			
			@Override
			public void run(Point _) {				
				final int nextGen = gen + 1;
				byte[][] inBoard = data[gen % 2];
				byte[][] outBoard = data[nextGen % 2];
				
				tile.run(inBoard, outBoard);
				
				if(nextGen < numGens) {
					points[nextGen % 2][tile.tr][tile.tc] =
						Intervals.siblingInterval(new TileTask(tile, nextGen)).end();
				}
			}

		}

		@Override
		byte[][] execute() {
			
			Intervals.blockingInterval(new AbstractTask() {
				public void run(Point _) {
					
					subdivide(rs, cs, w, h, new InvokableWithTile() {
						public void run(Tile t) {
							points[0][t.tr][t.tc] = 
								Intervals.childInterval(new TileTask(t, 0)).end();								
						}
					});
					
				}
			});
			
			return data[numGens % 2];
		}
	}

	/**
	 * A version which uses fine-grained dependencies.  
	 * Gens do not have to fully complete before the next
	 * gen can begin.    
	 */
	class FineGrainedIntervalEngine implements GameOfLifeEngine {
		
		public final int w, h;
		
		public FineGrainedIntervalEngine() {
			this(10, 10);
		}

		public FineGrainedIntervalEngine(int w, int h) {
			this.w = w;
			this.h = h;
		}

		@Override
		public byte[][] execute(byte[][] initialConfiguration, int rs, int cs,
				int numGens) {
			GameOfLifeBoard board = new FineGrainedIntervalBoard(initialConfiguration, rs, cs, numGens, w, h);
			return board.execute();
		}

	}
	
	/** Checks two byte arrays for equality. */
	private void assertConfigEqual(String desc, byte[][] expConfig, byte[][] actConfig) {
		if(DUMP_ALL_CONFIGS) {
			dumpConfig(desc + " expected", expConfig);
			dumpConfig(desc + " actual", actConfig);
		}
		
		for (int r = 1; r < expConfig.length; r++)
			for (int c = 1; c < expConfig[0].length; c++) {
				if(expConfig[r][c] != actConfig[r][c]) {
					dumpConfig(desc + " expected", expConfig);
					dumpConfig(desc + " actual", actConfig);
					Assert.assertEquals(
							String.format(
									"Error at %s (%d, %d).", desc, r, c),
							expConfig[r][c], 
							actConfig[r][c]);					
				}
			}
	}

	/** Prints the config to stderr */
	private void dumpConfig(String name, byte[][] expConfig) {
		System.err.println(name+":");
		
		System.err.print("  ");
		for (int c = 1; c < expConfig[0].length; c++)
			System.err.print(c % 10);
		System.err.println();
		
		for (int r = 1; r < expConfig.length; r++) {
			System.err.print(" ");
			System.err.print(r % 10);
			for (int c = 1; c < expConfig[0].length; c++)
				if(expConfig[r][c] == 0)
					System.err.print(".");
				else
					System.err.print("x");
			System.err.println();
		}
	}

	/** Prints the config to stderr */
	private void dumpConfig(String name, byte[][] expConfig, Tile tile) {
		System.err.printf("%s (%d,%d)-(%d,%d):\n", name, tile.r0, tile.c0, tile.rN, tile.cN);
		
		System.err.print("  ");
		for (int c = tile.c0; c <= tile.cN; c++)
			System.err.print(c % 10);
		System.err.println();
		
		for (int r = tile.r0; r <= tile.rN; r++) {
			System.err.print(" ");
			System.err.print(r % 10);
			for (int c = tile.c0; c <= tile.cN; c++)
				if(expConfig[r][c] == 0)
					System.err.print(".");
				else
					System.err.print("x");
			System.err.println();
		}
	}
	
	/** Little class used to structure sample patterns. */
	class PatternEvolution {
		final int gen;
		final byte[][] expected; /// 1-based!
		final int rs, cs;
		
		public PatternEvolution(int gen, int rs, int cs, byte[][] expected) {
			this.gen = gen;
			this.rs = rs;
			this.cs = cs;
			this.expected = expected;
		}
		
		/** Returns the same pattern but padded with blank space */
		public PatternEvolution paddedToSize(int rs1, int cs1) {
			byte[][] result = new byte[rs1+2][cs1+2];
			for(int r = 1; r <= rs; r++)
				System.arraycopy(expected[r], 1, result[r], 1, cs);
			return new PatternEvolution(gen, rs1, cs1, result);
		}
	}
	
	public PatternEvolution pattern(int gen, byte[][] in) {
		int rs = in.length, cs = in[0].length;
		byte[][] out = new byte[rs+2][cs+2];
		for(int r = 0; r < rs; r++)
			System.arraycopy(in[r], 0, out[r+1], 1, cs);
		return new PatternEvolution(gen, rs, cs, out);
	}
	
	/** Sample pattern called the Blinker which repeats after two gens. */
	PatternEvolution[] blinker = new PatternEvolution[] {
			
			pattern(0, new byte[][] {
					{ 0, 0, 0, },
					{ 1, 1, 1, },
					{ 0, 0, 0, }					
			}),
			pattern(1, new byte[][] {
					{ 0, 1, 0, },
					{ 0, 1, 0, },
					{ 0, 1, 0, }
   			}),
			pattern(2, new byte[][] {
					{ 0, 0, 0, },
					{ 1, 1, 1, },
					{ 0, 0, 0, }										
			})
			                       			                       			
	};

	/** Sample pattern called the Toad which repeats after two gens. */
	PatternEvolution[] theToad = new PatternEvolution[] {
			
			pattern(0, new byte[][] {
					{ 0, 0, 0, 0, }, 
					{ 0, 1, 1, 1, },
					{ 1, 1, 1, 0, },
					{ 0, 0, 0, 0, } 
			}),
			pattern(1, new byte[][] {
					{ 0, 0, 1, 0, }, 
					{ 1, 0, 0, 1, },
					{ 1, 0, 0, 1, },
					{ 0, 1, 0, 0, }
			}),
			pattern(2, new byte[][] {
					{ 0, 0, 0, 0, }, 
					{ 0, 1, 1, 1, },
					{ 1, 1, 1, 0, },
					{ 0, 0, 0, 0, } 										
			})
			                       			                       			
	};
	
	/** Sample pattern called the Queen Bee which runs indefinitely. */
	PatternEvolution[] queenBee = new PatternEvolution[] {
		pattern(0, new byte[][] {
				//0              5              10             15             20             30
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },								
		}),
		pattern(1, new byte[][] {
   				//0              5              10             15             20             30
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },				
   		}),
   		pattern(2, new byte[][] {
   				//0              5              10             15             20             30
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
   				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },				
   		}),
   		pattern(7, new byte[][] {
				//0              5              10             15             20             30
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },				
		}),
		pattern(37, new byte[][] {
				//0              5              10             15             20             30
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },				
		}),				
	};
	
	/** Sample pattern called the Gun which runs indefinitely and produces 
	 *  gliders that continue across the board.   */
	PatternEvolution[] gliderGun = new PatternEvolution[] {
		pattern(0, new byte[][] {
				//0              5              10             15             20             25             30             35
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0 },
				{ 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },								
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },								
		})
	};
	
	/**
	 * Compares the results from two board factories.  Always starts from the
	 * same initial pattern and runs for all gens between 1 and {@code maxGen},
	 * changing by {@code step}.  Do not modify.  
	 * @param minGen TODO
	 */
	void compareFactories(
			GameOfLifeEngine reference,
			GameOfLifeEngine test,
			PatternEvolution initialPattern, 
			int minGen, int maxGen, int step) 
	{
		for(int rnd = minGen; rnd < maxGen; rnd += step) {
			byte[][] refResult = reference.execute(initialPattern.expected, initialPattern.rs, initialPattern.cs, rnd);
			byte[][] testResult = test.execute(initialPattern.expected, initialPattern.rs, initialPattern.cs, rnd);			
			assertConfigEqual("Gen " + rnd, refResult, testResult);
		}
		
	}
	
	void timeFactory(
			GameOfLifeEngine test,
			PatternEvolution initialPattern, 
			int minGen, int maxGen, int step) 
	{
		double time0 = System.nanoTime() / 1e9;
		for(int rnd = minGen; rnd < maxGen; rnd += step) {
			test.execute(initialPattern.expected, initialPattern.rs, initialPattern.cs, rnd);			
		}
		double time1 = System.nanoTime() / 1e9;
		System.err.println(test.getClass().getName()+": "+(time1 - time0));
	}
	
	/**
	 * Given an array of patterns, check that {@code patterns[0]} 
	 * evolves into {@code patterns[i]} for all {@code i}.  Used only
	 * to test the serial board; from then on, we compare against the
	 * serial board. Do not modify.  
	 */
	void testPattern(String name, GameOfLifeEngine engine, PatternEvolution[] patterns) {
		PatternEvolution initialPattern = patterns[0];
		
		for(int i = 1; i < patterns.length; i++) {
			PatternEvolution finalPattern = patterns[i];
			byte[][] result = engine.execute(initialPattern.expected, initialPattern.rs, initialPattern.cs, finalPattern.gen);
			assertConfigEqual(
					name + " (gen "+finalPattern.gen+")", 
					finalPattern.expected, result);
		}
	}
	
	/** 
	 * Self Test: Checks that the serial execution is correct.
	 * Do not modify. 
	 */
	@Test public void serialQueenBee() {
		testPattern("Queen Bee", new SerialEngine(), queenBee);		
	}
	
	/** 
	 * Self Test: Checks that the serial execution is correct.
	 * Do not modify. 
	 */
	@Test public void serialBlinker() {
		testPattern("Blinker", new SerialEngine(), blinker);		
	}
	
	/** 
	 * Self Test: Checks that the serial execution is correct.
	 * Do not modify. 
	 */
	@Test public void serialTheToad() {
		testPattern("The Toad", new SerialEngine(), theToad);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Blinker pattern, which repeats every other gen.
	 */
	@Test public void serialTileBlinker() {
		compareFactories(
				new SerialEngine(), 
				new SerialTiledEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Toad pattern, which repeats every other gen.
	 */	
	@Test public void serialTileTheToad() {
		compareFactories(
				new SerialEngine(), 
				new SerialTiledEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void serialTileQueenBee() {
		compareFactories(
				new SerialEngine(), 
				new SerialTiledEngine(), 
				queenBee[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void serialTileGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new SerialTiledEngine(), 
				gliderGun[0], 
				1, 150, 1);	
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Blinker pattern, which repeats every other gen.
	 */
	@Test public void phasedBlinker() {
		compareFactories(
				new SerialEngine(), 
				new PhasedIntervalEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Toad pattern, which repeats every other gen.
	 */	
	@Test public void phasedTheToad() {
		compareFactories(
				new SerialEngine(), 
				new PhasedIntervalEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void phasedQueenBee() {
		compareFactories(
				new SerialEngine(), 
				new PhasedIntervalEngine(),
				queenBee[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void phasedGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new PhasedIntervalEngine(),
				gliderGun[0], 
				1, 150, 1);	
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Blinker pattern, which repeats every other gen.
	 */
	@Test public void simplerPhasedBlinker() {
		compareFactories(
				new SerialEngine(), 
				new SimplerPhasedIntervalEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Toad pattern, which repeats every other gen.
	 */	
	@Test public void simplerPhasedTheToad() {
		compareFactories(
				new SerialEngine(), 
				new SimplerPhasedIntervalEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void simplerPhasedQueenBee() {
		compareFactories(
				new SerialEngine(), 
				new SimplerPhasedIntervalEngine(),
				queenBee[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void simplerPhasedGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new SimplerPhasedIntervalEngine(),
				gliderGun[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Blinker pattern, which repeats every other gen.
	 */
	@Test public void indexedBlinker() {
		compareFactories(
				new SerialEngine(), 
				new IndexedIntervalEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Toad pattern, which repeats every other gen.
	 */	
	@Test public void indexedTheToad() {
		compareFactories(
				new SerialEngine(), 
				new IndexedIntervalEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void indexedQueenBee() {
		compareFactories(
				new SerialEngine(), 
				new IndexedIntervalEngine(),
				queenBee[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void indexedGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new IndexedIntervalEngine(),
				gliderGun[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Blinker pattern, which repeats every other gen.
	 */
	@Test public void fgBlinker() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Toad pattern, which repeats every other gen.
	 */	
	@Test public void fgTheToad() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void fgQueenBee() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(),
				queenBee[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void fgGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(),
				gliderGun[0], 
				1, 150, 1);
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Blinker pattern, which repeats every other gen.
	 */
	@Test public void barrierBlinker() {
		compareFactories(
				new SerialEngine(), 
				new BarrierEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 gens
	 * of the Toad pattern, which repeats every other gen.
	 */	
	@Test public void barrierTheToad() {
		compareFactories(
				new SerialEngine(), 
				new BarrierEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void barrierQueenBee() {
		compareFactories(
				new SerialEngine(), 
				new BarrierEngine(),
				queenBee[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 150 gens
	 * of the Queen Bee pattern.
	 */
	@Test public void barrierGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new BarrierEngine(),
				gliderGun[0], 
				1, 150, 1);
	}
	
	public void profile(String args[]) {
		for(int i = 0; i < args.length; i += 9) {
			String engineName = args[i];
			int tw = Integer.valueOf(args[i+1]);
			int th = Integer.valueOf(args[i+2]);
			String patternName = args[i+3];
			int minGen = Integer.valueOf(args[i+4]);
			int maxGen = Integer.valueOf(args[i+5]);
			int step = Integer.valueOf(args[i+6]);
			int bw = Integer.valueOf(args[i+7]);
			int bh = Integer.valueOf(args[i+8]);
			
			GameOfLifeEngine engine;			
			if(engineName.equals("serial")) engine = new SerialEngine();
			else if(engineName.equals("st")) engine = new SerialTiledEngine(tw, th);
			else if(engineName.equals("fg")) engine = new FineGrainedIntervalEngine(tw, th);
			else if(engineName.equals("b")) engine = new BarrierEngine();
			else if(engineName.equals("p")) engine = new PhasedIntervalEngine(tw, th);
			else if(engineName.equals("sp")) engine = new SimplerPhasedIntervalEngine(tw, th);
			else if(engineName.equals("i")) engine = new IndexedIntervalEngine();
			else throw new RuntimeException("Unknown engine: "+engineName);
			
			PatternEvolution pattern;
			if(patternName.equals("qb")) pattern = queenBee[0];
			else if(patternName.equals("gg")) pattern = gliderGun[0];
			else throw new RuntimeException("Unknown pattern: "+patternName);
			
			pattern = pattern.paddedToSize(bh, bw);
						
			// Warm up gen:
			timeFactory(engine, pattern, minGen, minGen + 1, 1);
			
			// Real run:
			timeFactory(engine, pattern, minGen, maxGen, step);			
		}
		
		executorService.shutdown();
	}
	
	public static void main(String args[]) {
		new TestGameOfLife().profile(args);
	}

}
