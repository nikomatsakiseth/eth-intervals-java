package ch.ethz.intervals.visualizer;

import java.util.BitSet;
import java.util.Iterator;
import java.util.LinkedList;

public class PointInfo {
	
	public final IntervalInfo inter;
	public final int id;
	protected EdgeInfo incoming; // read by GraphWalker, otherwise private
	protected EdgeInfo outgoing; // read by GraphWalker, otherwise private
	public int arrivalTime = Integer.MAX_VALUE;  /** set when it arrives */
	public boolean needsLayout; /** used by layout algorithms */
	
	public PointInfo(IntervalInfo inter, int id) {
		this.inter = inter;
		this.id = id;
	}
	
	public boolean isStart() {
		return inter.start == this;
	}
	
	public void connectTo(PointInfo tar) {		
		outgoing = tar.incoming = new EdgeInfo(
				this, 
				tar,
				outgoing,
				tar.incoming);
	}

	public void adjustSuperfluousSucc(
			int amnt,
			PointInfo tar) 
	{
		for(EdgeInfo edge : succEdges()) {
			if(edge.to == tar) {
				// No need to walk to our predecessors: 
				//   Any superfluous edges they have are already marked by us!
				edge.superfluous += amnt;
				return;
			}
		}
		
		for(EdgeInfo edge : predEdges())
			edge.from.adjustSuperfluousSucc(amnt, tar);
	}
	
	private void removeIncoming(EdgeInfo rem) {
		EdgeInfo prev = null, e = incoming;
		while(e != null) {
			if(e == rem) {
				if(prev == null)
					incoming = rem.nextIn;
				else
					prev.nextIn = rem.nextIn;
				return;
			}
			
			prev = e;
			e = e.nextIn;
		}
		
		// not in the list
	}
	
	private void removeOutgoing(EdgeInfo rem) {
		EdgeInfo prev = null, e = outgoing;
		while(e != null) {
			if(e == rem) {
				if(prev == null)
					outgoing = rem.nextOut;
				else
					prev.nextOut = rem.nextOut;
				return;
			}
			
			prev = e;
			e = e.nextOut;
		}
		
		// not in the list
	}
	
	public void remove() {
		for(EdgeInfo edge = outgoing; edge != null; edge = edge.nextOut)
			edge.to.removeIncoming(edge);
		for(EdgeInfo edge = incoming; edge != null; edge = edge.nextIn)
			edge.from.removeOutgoing(edge);
		outgoing = incoming = null;
	}
	
	@Override
	public String toString() {
		return String.format("Point(%d,%s(%d))", 
				this.id,
				(isStart() ? "Start" : "End"),
				inter.id);
	}
	
	public Iterable<EdgeInfo> edges(final GraphWalker walker) {
		return new Iterable<EdgeInfo>() {
			
			@Override
			public Iterator<EdgeInfo> iterator() {
				return new Iterator<EdgeInfo>() {
					
					EdgeInfo edge = walker.edges(PointInfo.this);

					@Override
					public boolean hasNext() {
						return (edge != null);
					}

					@Override
					public EdgeInfo next() {
						EdgeInfo res = edge;
						edge = walker.nextEdge(edge);
						return res;
					}

					@Override
					public void remove() {
						throw new UnsupportedOperationException();
					}
				};
			}
		};
	}
	
	public Iterable<PointInfo> points(
			final GraphWalker walker, 
			final boolean all) 
	{
		return new FilteredIterable<EdgeInfo, PointInfo>(edges(walker)) {
			
			@Override
			protected PointInfo preload() {
				if(all)
					return walker.opp(PointInfo.this);
				return null;
			}

			@Override
			protected PointInfo map(EdgeInfo e) {
				return walker.point(e);
			}

			@Override
			protected boolean shouldInclude(EdgeInfo e) {
				return true;
			}

		};
	}
	
	public Iterable<PointInfo> nonSuperfluousPlacedPoints(final GraphWalker walker) {
		return new FilteredIterable<EdgeInfo, PointInfo>(edges(walker)) {

			@Override
			protected PointInfo map(EdgeInfo e) {
				return walker.point(e);
			}

			@Override
			protected boolean shouldInclude(EdgeInfo e) {
				return e.superfluous == 0 && map(e).inter.bounds != null;
			}

		};
	}
	
	public Iterable<EdgeInfo> succEdges() {
		return edges(GraphWalker.forward);
	}
	
	public Iterable<EdgeInfo> predEdges() {
		return edges(GraphWalker.reverse);
	}
	
	public Iterable<PointInfo> succs() {
		return points(GraphWalker.forward, false);
	}
		
	public Iterable<PointInfo> preds() {
		return points(GraphWalker.reverse, false);
	}
	
	public Iterable<PointInfo> allSuccs() {
		return points(GraphWalker.forward, true);
	}
		
	public Iterable<PointInfo> allPreds() {
		return points(GraphWalker.reverse, true);
	}
	
	public Iterable<PointInfo> nonSuperfluousPlacedSuccs() {
		return nonSuperfluousPlacedPoints(GraphWalker.forward);
	}
	
	public Iterable<PointInfo> nonSuperfluousPlacedPreds() {
		return nonSuperfluousPlacedPoints(GraphWalker.reverse);
	}
	
	/**
	 * During layout, we try to place all preds first, but sometimes it happens than 
	 * new interval construction (for example) is interrupted, and there may be
	 * points belonging to partially constructed intervals that have not yet been placed.
	 * This routine just filters them out.
	 */
	public Iterable<PointInfo> placedPreds() {
		return new FilteredIterable<EdgeInfo, PointInfo>(predEdges()) {

			@Override
			protected PointInfo map(EdgeInfo e) {
				return e.from;
			}

			@Override
			protected boolean shouldInclude(EdgeInfo e) {
				return e.from.inter.bounds != null;
			}

		};
	}

	public boolean hb(final PointInfo tar) {
		if(tar == this)
			return false;
		
		class VisitQueue {
			BitSet visited = new BitSet();
			LinkedList<PointInfo> q = new LinkedList<PointInfo>();
			
			boolean addAll(Iterable<PointInfo> pnts) {
				for(PointInfo succ : pnts) {
					if(succ == tar)
						return true;
					if(!visited.get(succ.id)) {
						visited.set(succ.id);
						q.add(succ);
					}					
				}
				
				return false;				
			}
		}
		VisitQueue q = new VisitQueue();
		
		if(q.addAll(allSuccs()))
			return true;
		
		while(!q.q.isEmpty()) {
			PointInfo r = q.q.removeFirst();
			if(q.addAll(r.allSuccs()))
				return true;
		}
		
		return false;
	}
	
}
