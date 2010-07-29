package ch.ethz.intervals.ddl;

import java.util.ArrayList;
import java.util.List;

import ch.ethz.intervals.Intervals;

import ch.ethz.intervals.Lock;

public class GameBoard 
{
	public static final int TL = 0;
	public static final int BL = 1;
	public static final int TR = 2;
	public static final int BR = 3;
	
	public final int x, y, size;
	public final GameBoard subboards[];
	public final List<Player> players = new ArrayList<Player>();
	public final Lock lock;
	
	private GameBoard(int x, int y, int size, GameBoard[] subboards) {
		this.x = x;
		this.y = y;
		this.size = size;
		this.subboards = subboards;
		this.lock = Intervals.lock(String.format("GB-%d-%d-%d", x, y, size));
	}
	
	public final boolean isLeaf() {
		return subboards == null;
	}
	
	public static GameBoard makeBoard(
			int x, 
			int y, 
			int size,
			int leafSize
	) {
		if(size <= leafSize) {
			return new GameBoard(x, y, size, null);
		} else {
			GameBoard[] subboards = new GameBoard[4];
			int halfSize = size / 2;
			subboards[TL] = makeBoard(x, y, halfSize, leafSize);
			subboards[TR] = makeBoard(x + halfSize, y, halfSize, leafSize);
			subboards[BL] = makeBoard(x, y + halfSize, halfSize, leafSize);
			subboards[BR] = makeBoard(x + halfSize, y + halfSize, halfSize, leafSize);
			return new GameBoard(x, y, size, subboards);
		}
	}
	
	public final boolean contains(int aX, int aY, int aSize) {
		return (x <= aX && 
				y <= aY &&
				x + size >= aX + aSize &&
				y + size >= aY + aSize);
	}
	
	public final void addToThis(Player p) {
		p.checkWritable();
		
		assert(p.node == null);
		assert(contains(p.x, p.y, p.size));
		
	}

}
