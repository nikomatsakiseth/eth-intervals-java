package ch.ethz.intervals;

import junit.framework.Assert;

import org.junit.Test;

/**
 * ------------------------------------------------------------------------------------
 * READ ME FIRST
 * 
 * This file is intended to be used as a JUnit 4 test.  In Eclipse, you can simply 
 * right-click on the class and choose Run As > JUnit Test.  (You may have to add 
 * JUnit 4 to the project's build path.  To do that, right-click the project and select
 * Properties > Java Build Path.  From there, choose Libraries > Add Library and select
 * JUnit version 4.  Alternatively, you can download the JUnit 4 jar file and add it to the
 * project as well)
 * 
 * It contains a reference engine {@link TestGameOfLife.SerialEngine} which performs the 
 * game of life in a serial, round-based fashion, and a 
 * placeholder class {@link TestGameOfLife.PhasedIntervalEngine} 
 * for you to implement your own Intervals-based engine.  
 * Feel free to create more than one intervals-based engine to test multiple strategies:
 * you simply need to create additional classes implementing the
 * {@link TestGameOfLife.GameOfLifeEngine} interface and add appropriate tests.
 * 
 * There are currently 3 sample patterns.  You can of course add more.  The 
 * tests serialBlinker, serialTheToad, and serialQueenBee check that the 
 * serial engine, when using each of these patterns,
 * gets the expected results.  As you won't be modifying the serial engine,
 * these are not for you to use, they are just a sanity
 * check.
 * 
 * The next three tests (intervalBlinker, etc) check your Interval-based engine 
 * against the serial one. These should help you to debug.  They currently print 
 * out the board whenever testing fails; if you set the boolean flag 
 * {@link TestGameOfLife#DUMP_ALL_CONFIGS} (below) to true, they will dump out the board
 * after each step, which is sometimes helpful. 
 * 
 * The game of life board is represented as a square, 2D array of bytes: byte[][].
 * Each cell is either 0 (dead) or 1 (alive).  
 * ------------------------------------------------------------------------------------
 */
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

	/** Simple abstraction layer for testing and comparing different engines */ 
	interface GameOfLifeEngine {
		/** Runs the game of life given the initial configuration {@code config},
		 *  which must be a square matrix.  The routine should execute
		 *  {@code maxRound} rounds and return the final configuration. */
		byte[][] execute(byte[][] initialConfiguration, int maxRound);
	}
	
	class SerialEngine implements GameOfLifeEngine {

		class SerialBoard {
			
			final int rs, cs, maxRound;
			final byte[][][] data;
			
			public SerialBoard(byte[][] data, int maxRound) {
				this.rs = data.length;
				this.cs = data[0].length;
				this.maxRound = maxRound;
				this.data = new byte[2][rs][cs];
				
				for(int r = 0; r < rs; r++)
					for(int c = 0; c < cs; c++)
						this.data[0][r][c] = data[r][c];
			}
			
			public int total3(byte[][] inData, int r, int c) {
				int total = 0;
				if(r >= 0 && r < rs) {
					if(c > 0) total += inData[r][c - 1];
					total += inData[r][c];
					if(c + 1 < cs) total += inData[r][c+1];
				}
				return total;
			}

			public void execute() {
				for(int rnd = 1; rnd <= maxRound; rnd++) {
					int mrnd = rnd % 2;
					byte outData[][] = data[mrnd];
					byte inData[][] = data[1 - mrnd];
					
					for(int r = 0; r < rs; r++) {
						for(int c = 0; c < cs; c++) {
							int total = 0;						
							total += total3(inData, r - 1, c);
							total += total3(inData, r, c) - inData[r][c];
							total += total3(inData, r + 1, c);						
							outData[r][c] = gameOfLife(inData[r][c], total);
						}
					}
							
				}
			}

		}
		
		@Override
		public byte[][] execute(byte[][] initialConfiguration, int maxRound) {
			SerialBoard sb = new SerialBoard(initialConfiguration, maxRound);
			sb.execute();
			return sb.data[maxRound % 2];
		}

	}
	
	class Tile {
		public final int tr, tc; /** tile row, tile column */
		public final int r0, rN; /** first, last row + 1   */
		public final int c0, cN; /** first, last col + 1   */
		
		public Tile(int tr, int tc, int r0, int rN, int c0, int cN) {
			this.tr = tr;
			this.tc = tc;
			this.r0 = r0;
			this.rN = rN;
			this.c0 = c0;
			this.cN = cN;
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

		class SerialBoard {
			
			final int rs, cs, maxRound;
			final byte[][][] data;
			
			public SerialBoard(byte[][] data, int maxRound) {
				this.rs = data.length;
				this.cs = data[0].length;
				this.maxRound = maxRound;
				this.data = new byte[2][rs][cs];
				
				for(int r = 0; r < rs; r++)
					for(int c = 0; c < cs; c++)
						this.data[0][r][c] = data[r][c];
			}
			
			public int total3(byte[][] inData, int r, int c) {
				int total = 0;
				if(r >= 0 && r < rs) {
					if(c > 0) total += inData[r][c - 1];
					total += inData[r][c];
					if(c + 1 < cs) total += inData[r][c+1];
				}
				return total;
			}

			public void execute() {
				for(int rnd = 1; rnd <= maxRound; rnd++) {
					int mrnd = rnd % 2;
					byte outData[][] = data[mrnd];
					byte inData[][] = data[1 - mrnd];
					
					for(int r0 = 0; r0 < rs; r0 += h) {
						for(int c0 = 0; c0 < cs; c0 += w) {
							int rN = Math.min(r0 + h, rs);
							int cN = Math.min(c0 + w, cs);
							for(int r = r0; r < rN; r++) {
								for(int c = c0; c < cN; c++) {
									int total = 0;						
									total += total3(inData, r - 1, c);
									total += total3(inData, r, c) - inData[r][c];
									total += total3(inData, r + 1, c);						
									outData[r][c] = gameOfLife(inData[r][c], total);
								}
							}
						}
					}
							
				}
			}

		}
		
		@Override
		public byte[][] execute(byte[][] initialConfiguration, int maxRound) {
			SerialBoard sb = new SerialBoard(initialConfiguration, maxRound);
			sb.execute();
			return sb.data[maxRound % 2];
		}

	}
	
	/**
	 * A version that uses phases.  Each round fully completes before
	 * the next begins.  The structure follows a generic barrier
	 * pattern, where at any given moment there is an interval
	 * {@code nextRound} where processing for the next round is
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
		
		class PhasedIntervalBoard {
			public final int rs, cs;
			public Interval nextRound;
			public int roundsRemaining;
			
			public PhasedIntervalBoard(int rs, int cs, int roundsRemaining, Point endOfThisRound) 
			{
				this.rs = rs;
				this.cs = cs;
				this.roundsRemaining = roundsRemaining;
				switchRounds(endOfThisRound);
			}

			void switchRounds(Point endOfThisRound) {
				Interval switchInterval =
					Intervals.intervalWithBound(endOfThisRound.bound())
						.startAfter(endOfThisRound)
						.schedule(switchRoundTask);
				
				nextRound = 
					Intervals.intervalWithBound(endOfThisRound.bound())
						.startAfter(switchInterval.end())
						.schedule(Intervals.emptyTask);
			}
			
			class SwitchRoundTask implements Task {

				@Override
				public void run(Point currentEnd) {
					if(roundsRemaining-- > 0)
						switchRounds(nextRound.end());
				}
				
			}
			final SwitchRoundTask switchRoundTask = new SwitchRoundTask();
			
			public void startTile(Tile tile, byte[][] inBoard, byte[][] outBoard) {
				Intervals
					.intervalDuring(nextRound)
					.schedule(new TileTask(tile, inBoard, outBoard));
			}

			class TileTask implements Task {
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
				
				public int total3(byte[][] inData, int r, int c) {
					int total = 0;
					if(r >= 0 && r < rs) {
						if(c > 0) total += inData[r][c - 1];
						total += inData[r][c];
						if(c + 1 < cs) total += inData[r][c+1];
					}
					return total;
				}				
				
				@Override
				public void run(Point currentEnd) {
					for(int r = tile.r0; r < tile.rN; r++)
						for(int c = tile.c0; c < tile.cN; c++) {
							int total = 0;
							total += total3(inBoard, r - 1, c);
							total += total3(inBoard, r, c) - inBoard[r][c];
							total += total3(inBoard, r + 1, c);								
							outBoard[r][c] = gameOfLife(inBoard[r][c], total);
						}
					
					if(roundsRemaining > 0)
						startTile(tile, outBoard, inBoard);					
				}
			}
		}
		
		@Override
		public byte[][] execute(byte[][] initialConfig, final int maxRound) {
			final int rs = initialConfig.length;
			final int cs = initialConfig[0].length;
			final byte[][][] boards = new byte[2][rs][cs];

			for(int r = 0; r < rs; r++)
				System.arraycopy(initialConfig[r], 0, boards[0][r], 0, cs);
			
			Intervals.blockingInterval(new SubintervalTask(new Task() {

				@Override
				public void run(Point currentEnd) {
					PhasedIntervalBoard piBoard = new PhasedIntervalBoard(rs, cs, maxRound, currentEnd);
					for(int r = 0; r < rs; r += h)
						for(int c = 0; c < cs; c += w) {
							int rN = Math.min(r + h, rs);
							int cN = Math.min(c + w, cs);
							Tile tile = new Tile(r / h, c / w, r, rN, c, cN);
							piBoard.startTile(tile, boards[0], boards[1]);
						}
				}
				
			}));
			
			return boards[maxRound % 2];
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
		
		class SimplerPhasedIntervalBoard {
			public final int rs, cs;
			
			public SimplerPhasedIntervalBoard(int rs, int cs) {
				this.rs = rs;
				this.cs = cs;
			}

			public int total3(byte[][] inData, int r, int c) {
				int total = 0;
				if(r >= 0 && r < rs) {
					if(c > 0) total += inData[r][c - 1];
					total += inData[r][c];
					if(c + 1 < cs) total += inData[r][c+1];
				}
				return total;
			}	
			
			public byte[][] compute(byte[][] initialConfig, final int maxRound) {
				final byte boards[][][] = new byte[2][rs][cs];
				
				for(int r = 0; r < rs; r++)
					System.arraycopy(initialConfig[r], 0, boards[0][r], 0, cs);
				
				// using an outer interval is a minor optimization, because it's
				// expensive to switch from the "main" Java thread to the interval
				// worker threads.
				Intervals.blockingInterval(new Task() {
					public void run(Point _) {
						for(int rnd = 0; rnd < maxRound; rnd++) {
							final byte[][] inBoard = boards[rnd % 2];
							final byte[][] outBoard = boards[1 - (rnd % 2)];
							
							Intervals.blockingInterval(new Task() {
								public void run(Point currentEnd) {
									for(int r = 0; r < rs; r += h)
										for(int c = 0; c < cs; c += w) {
											final int r0 = r;
											final int c0 = c;
											final int rN = Math.min(r + h, rs);
											final int cN = Math.min(c + w, cs);
											
											Intervals.intervalWithBound(currentEnd).schedule(new Task() {
												public String toString() 
												{
													return "Tile("+r0/h+","+c0/w+")";
												}
		
												public void run(Point currentEnd) {
													
													for(int ri = r0; ri < rN; ri++)
														for(int ci = r0; ci < cN; ci++) {
															int total = 0;
															total += total3(inBoard, ri - 1, ci);
															total += total3(inBoard, ri, ci) - inBoard[ri][ci];
															total += total3(inBoard, ri + 1, ci);								
															outBoard[ri][ci] = gameOfLife(inBoard[ri][ci], total);
														}
												}
											});
										}
								}
							});
						}
					}
				});	
				return boards[maxRound % 2];
			}
		}
		
		@Override
		public byte[][] execute(byte[][] initialConfig, final int maxRound) {
			final int rs = initialConfig.length;
			final int cs = initialConfig[0].length;
			SimplerPhasedIntervalBoard sib = new SimplerPhasedIntervalBoard(rs, cs);
			return sib.compute(initialConfig, maxRound);
		}		
	}
	
	/**
	 * A version which uses fine-grained dependencies.  
	 * Rounds do not have to fully complete before the next
	 * round can begin.    
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

		class FineGrainedIntervalBoard {
			public final int rs, cs;
			
			public FineGrainedIntervalBoard(int rs, int cs)
			{
				this.rs = rs;
				this.cs = cs;
			}

			class TileTask implements Task {
				public final Tile tile;
				public final int roundsRemaining;
				public final Point[][] inIntervals;
				public final byte[][] inBoard;
				public final Point[][] outIntervals;
				public final byte[][] outBoard;
				
				public TileTask(
						Tile tile, 
						int roundsRemaining,
						Point[][] inIntervals, byte[][] inBoard,
						Point[][] outIntervals, byte[][] outBoard)
				{
					this.tile = tile;
					this.roundsRemaining = roundsRemaining;
					this.inIntervals = inIntervals;
					this.inBoard = inBoard;
					this.outIntervals = outIntervals;
					this.outBoard = outBoard;
				}
				
				public String toString() 
				{
					return "Tile("+tile.tr+","+tile.tc+")";
				}

				public int total3(byte[][] inData, int r, int c) {
					int total = 0;
					if(r >= 0 && r < rs) {
						if(c > 0) total += inData[r][c - 1];
						total += inData[r][c];
						if(c + 1 < cs) total += inData[r][c+1];
					}
					return total;
				}				
				
				@Override
				public void run(Point currentEnd) {
					
					for(int r = tile.r0; r < tile.rN; r++)
						for(int c = tile.c0; c < tile.cN; c++) {
							int total = 0;
							total += total3(inBoard, r - 1, c);
							total += total3(inBoard, r, c) - inBoard[r][c];
							total += total3(inBoard, r + 1, c);								
							outBoard[r][c] = gameOfLife(inBoard[r][c], total);
						}
					
					if(roundsRemaining > 0) {
						// n.b.: The inIntervals and outIntervals arrays are padded with nulls
						//       at the boundaries, and therefore use a 1-based offset!
						outIntervals[tile.tr+1][tile.tc+1] =
							Intervals.intervalWithBound(currentEnd.bound())
							.startAfter(inIntervals[tile.tr+0][tile.tc+0])
							.startAfter(inIntervals[tile.tr+1][tile.tc+0])
							.startAfter(inIntervals[tile.tr+2][tile.tc+0])
							.startAfter(inIntervals[tile.tr+0][tile.tc+1])
							.startAfter(inIntervals[tile.tr+2][tile.tc+1])
							.startAfter(inIntervals[tile.tr+0][tile.tc+2])
							.startAfter(inIntervals[tile.tr+1][tile.tc+2])
							.startAfter(inIntervals[tile.tr+2][tile.tc+2])
							.schedule(new TileTask(tile, roundsRemaining - 1, outIntervals, outBoard, inIntervals, inBoard))
							.end();
					}
				}
			}
		}
		
		@Override
		public byte[][] execute(byte[][] initialConfig, final int maxRound) {
			final int rs = initialConfig.length;
			final int cs = initialConfig[0].length;
			final byte[][][] boards = new byte[2][rs][cs];
			final Point[][][] intervals = new Point[2][rs+2][cs+2];

			for(int r = 0; r < rs; r++)
				System.arraycopy(initialConfig[r], 0, boards[0][r], 0, cs);
			
			Intervals.blockingInterval(new SetupTask() {
				@Override
				public void setup(Point currentEnd, Interval worker) {
					FineGrainedIntervalBoard fgBoard = new FineGrainedIntervalBoard(rs, cs);
					for(int r = 0; r < rs; r += h)
						for(int c = 0; c < cs; c += w) {
							int rN = Math.min(r + h, rs);
							int cN = Math.min(c + w, cs);
							Tile tile = new Tile(r / h, c / h, r, rN, c, cN);
							intervals[0][tile.tr+1][tile.tc+1] =
								Intervals.intervalDuring(worker)
								.schedule(fgBoard.new TileTask(tile, maxRound, intervals[0], boards[0], intervals[1], boards[1]))
								.end();
						}
				}
			});
			
			return boards[maxRound % 2];
		}		
	}
	
	/** Checks two byte arrays for equality. */
	private void assertConfigEqual(String desc, byte[][] expConfig, byte[][] actConfig) {
		if(DUMP_ALL_CONFIGS) {
			dumpConfig(desc + " expected", expConfig);
			dumpConfig(desc + " actual", actConfig);
		}
		
		for (int r = 0; r < expConfig.length; r++)
			for (int c = 0; c < expConfig[0].length; c++) {
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
		for (int c = 0; c < expConfig[0].length; c++)
			System.err.print(c % 10);
		System.err.println();
		
		for (int r = 0; r < expConfig.length; r++) {
			System.err.print(" ");
			System.err.print(r % 10);
			for (int c = 0; c < expConfig[0].length; c++)
				if(expConfig[r][c] == 0)
					System.err.print(".");
				else
					System.err.print("x");
			System.err.println();
		}
	}

	/** Little class used to structure sample patterns. */
	class PatternEvolution {
		final int round;
		final byte[][] expected;
		public PatternEvolution(int round, byte[][] expected) {
			super();
			this.round = round;
			this.expected = expected;
		}
		
		/** Returns the same pattern but padded with blank space */
		public PatternEvolution paddedToSize(int rs, int cs) {
			byte[][] result = new byte[rs][cs];
			for(int r = 0; r < expected.length; r++)
				System.arraycopy(expected[r], 0, result[r], 0, expected[r].length);
			return new PatternEvolution(round, result);
		}
	}

	/** Sample pattern called the Blinker which repeats after two rounds. */
	PatternEvolution[] blinker = new PatternEvolution[] {
			
			new PatternEvolution(0, new byte[][] {
					{ 0, 0, 0, },
					{ 1, 1, 1, },
					{ 0, 0, 0, }					
			}),
			new PatternEvolution(1, new byte[][] {
					{ 0, 1, 0, },
					{ 0, 1, 0, },
					{ 0, 1, 0, }
   			}),
			new PatternEvolution(2, new byte[][] {
					{ 0, 0, 0, },
					{ 1, 1, 1, },
					{ 0, 0, 0, }										
			})
			                       			                       			
	};

	/** Sample pattern called the Toad which repeats after two rounds. */
	PatternEvolution[] theToad = new PatternEvolution[] {
			
			new PatternEvolution(0, new byte[][] {
					{ 0, 0, 0, 0, }, 
					{ 0, 1, 1, 1, },
					{ 1, 1, 1, 0, },
					{ 0, 0, 0, 0, } 
			}),
			new PatternEvolution(1, new byte[][] {
					{ 0, 0, 1, 0, }, 
					{ 1, 0, 0, 1, },
					{ 1, 0, 0, 1, },
					{ 0, 1, 0, 0, }
			}),
			new PatternEvolution(2, new byte[][] {
					{ 0, 0, 0, 0, }, 
					{ 0, 1, 1, 1, },
					{ 1, 1, 1, 0, },
					{ 0, 0, 0, 0, } 										
			})
			                       			                       			
	};
	
	/** Sample pattern called the Queen Bee which runs indefinitely. */
	PatternEvolution[] queenBee = new PatternEvolution[] {
		new PatternEvolution(0, new byte[][] {
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
		new PatternEvolution(1, new byte[][] {
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
   		new PatternEvolution(2, new byte[][] {
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
   		new PatternEvolution(7, new byte[][] {
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
		new PatternEvolution(37, new byte[][] {
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
		new PatternEvolution(0, new byte[][] {
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
	 * same initial pattern and runs for all rounds between 1 and {@code maxRound},
	 * changing by {@code step}.  Do not modify.  
	 * @param minRound TODO
	 */
	void compareFactories(
			GameOfLifeEngine reference,
			GameOfLifeEngine test,
			PatternEvolution initialPattern, 
			int minRound, int maxRound, int step) 
	{
		for(int rnd = minRound; rnd < maxRound; rnd += step) {
			byte[][] refResult = reference.execute(initialPattern.expected, rnd);
			byte[][] testResult = test.execute(initialPattern.expected, rnd);			
			assertConfigEqual("Round " + rnd, refResult, testResult);
		}
		
	}
	
	void timeFactory(
			GameOfLifeEngine test,
			PatternEvolution initialPattern, 
			int minRound, int maxRound, int step) 
	{
		double time0 = System.nanoTime() / 1e9;
		for(int rnd = minRound; rnd < maxRound; rnd += step) {
			test.execute(initialPattern.expected, rnd);			
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
			byte[][] result = engine.execute(initialPattern.expected, finalPattern.round);
			assertConfigEqual(
					name + " (round "+finalPattern.round+")", 
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
	 * Compares the interval board against the serial board for 3 rounds
	 * of the Blinker pattern, which repeats every other round.
	 */
	@Test public void serialTileBlinker() {
		compareFactories(
				new SerialEngine(), 
				new SerialTiledEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 rounds
	 * of the Toad pattern, which repeats every other round.
	 */	
	@Test public void serialTileTheToad() {
		compareFactories(
				new SerialEngine(), 
				new SerialTiledEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 rounds
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
	 * Compares the interval board against the serial board for 150 rounds
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
	 * Compares the interval board against the serial board for 3 rounds
	 * of the Blinker pattern, which repeats every other round.
	 */
	@Test public void phasedBlinker() {
		compareFactories(
				new SerialEngine(), 
				new PhasedIntervalEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 rounds
	 * of the Toad pattern, which repeats every other round.
	 */	
	@Test public void phasedTheToad() {
		compareFactories(
				new SerialEngine(), 
				new PhasedIntervalEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 rounds
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
	 * Compares the interval board against the serial board for 150 rounds
	 * of the Queen Bee pattern.
	 */
	@Test public void phasedGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(),
				gliderGun[0], 
				1, 150, 1);	
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 rounds
	 * of the Blinker pattern, which repeats every other round.
	 */
	@Test public void simplerPhasedBlinker() {
		compareFactories(
				new SerialEngine(), 
				new SimplerPhasedIntervalEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 rounds
	 * of the Toad pattern, which repeats every other round.
	 */	
	@Test public void simplerPhasedTheToad() {
		compareFactories(
				new SerialEngine(), 
				new SimplerPhasedIntervalEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 rounds
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
	 * Compares the interval board against the serial board for 150 rounds
	 * of the Queen Bee pattern.
	 */
	@Test public void simplerPhasedGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(),
				gliderGun[0], 
				1, 150, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 rounds
	 * of the Blinker pattern, which repeats every other round.
	 */
	@Test public void fgBlinker() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(), 
				blinker[0], 
				1, 3, 1);		
	}
	
	/** 
	 * Compares the interval board against the serial board for 3 rounds
	 * of the Toad pattern, which repeats every other round.
	 */	
	@Test public void fgTheToad() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(), 
				theToad[0], 
				1, 3, 1);		
	}
		
	
	/** 
	 * Compares the interval board against the serial board for 150 rounds
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
	 * Compares the interval board against the serial board for 150 rounds
	 * of the Queen Bee pattern.
	 */
	@Test public void fgGliderGun() {
		compareFactories(
				new SerialEngine(), 
				new FineGrainedIntervalEngine(),
				gliderGun[0], 
				1, 150, 1);
	}
	
	public void profile(String args[]) {
		for(int i = 0; i < args.length; i += 9) {
			String engineName = args[i];
			int tw = Integer.valueOf(args[i+1]);
			int th = Integer.valueOf(args[i+2]);
			String patternName = args[i+3];
			int minRound = Integer.valueOf(args[i+4]);
			int maxRound = Integer.valueOf(args[i+5]);
			int step = Integer.valueOf(args[i+6]);
			int bw = Integer.valueOf(args[i+7]);
			int bh = Integer.valueOf(args[i+8]);
			
			GameOfLifeEngine engine;			
			if(engineName.equals("serial")) engine = new SerialEngine();
			else if(engineName.equals("st")) engine = new SerialTiledEngine(tw, th);
			else if(engineName.equals("fg")) engine = new FineGrainedIntervalEngine(tw, th);
			else if(engineName.equals("p")) engine = new PhasedIntervalEngine(tw, th);
			else if(engineName.equals("sp")) engine = new SimplerPhasedIntervalEngine(tw, th);
			else throw new RuntimeException("Unknown engine: "+engineName);
			
			PatternEvolution pattern;
			if(patternName.equals("qb")) pattern = queenBee[0];
			else if(patternName.equals("gg")) pattern = gliderGun[0];
			else throw new RuntimeException("Unknown pattern: "+patternName);
			
			pattern = pattern.paddedToSize(bh, bw);
						
			// Warm up round:
			timeFactory(engine, pattern, minRound, minRound + 1, 1);
			
			// Real run:
			timeFactory(engine, pattern, minRound, maxRound, step);			
		}
		
	}
	
	public static void main(String args[]) {
		new TestGameOfLife().profile(args);
	}
	
}
