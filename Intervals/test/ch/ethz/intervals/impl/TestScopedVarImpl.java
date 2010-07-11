package ch.ethz.intervals.impl;

import junit.framework.Assert;

import org.junit.Test;

import ch.ethz.intervals.Context;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.IntervalException.MustBeCurrent;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.ScopedVar;
import ch.ethz.intervals.task.AbstractTask;

public class TestScopedVarImpl extends TestUtil {

	@Test 
	public void testDefaultValue() {
		Context ctx = Intervals.context();
		ScopedVar<Double> d = ctx.scopedVar(2.2);
		Assert.assertEquals(2.2, d.get());
	}
	
	@Test(expected=IntervalException.MustBeCurrent.class)
	public void testIllegalSetByCompletedInline() {
		final Context ctx = Intervals.context();
		final ScopedVar<Integer> var = ctx.scopedVar(null);
		final Interval[] temp = new Interval[1];
		Intervals.inline(new AbstractTask("a") {
			@Override public void run(Interval a) throws Exception {
				temp[0] = a;
			}
		});
		var.set(temp[0], 5);
	}
	
	@Test(expected=IntervalException.MustBeCurrent.class)
	public void testIllegalGetByCompletedInline() {
		final Context ctx = Intervals.context();
		final ScopedVar<Integer> var = ctx.scopedVar(null);
		final Interval[] temp = new Interval[1];
		Intervals.inline(new AbstractTask("a") {
			@Override public void run(Interval a) throws Exception {
				temp[0] = a;
			}
		});
		var.get(temp[0]);
	}
	
	@Test
	public void testIllegalSetByAsync() {
		final Context ctx = Intervals.context();
		final ScopedVar<Integer> var = ctx.scopedVar(null);
		try {
			Intervals.inline(new AbstractTask("a") {
				@Override public void run(final Interval a) throws Exception {
					a.newAsyncChild(new AbstractTask("b") {
						@Override public void run(Interval c) throws Exception {
							var.set(a, 3);
						}					
					});
				}
			});
		} catch (RethrownException e) {
			assertThrew(e, MustBeCurrent.class);
		}
	}

	@Test 
	public void testInheritedValues() {
		final Context ctx = Intervals.context();
		final ScopedVar<Integer> var = ctx.scopedVar(null);
		
		Intervals.inline(new AbstractTask("a") {
			@Override public void run(final Interval a) {
				var.set(a, 3);
				Assert.assertEquals(Integer.valueOf(3), var.get());
				
				Intervals.inline(new AbstractTask("b") {
					@Override public void run(final Interval b) {
						Assert.assertEquals(Integer.valueOf(3), var.get());
						var.set(b, 4);
						Assert.assertEquals(Integer.valueOf(4), var.get());						
						Assert.assertEquals(Integer.valueOf(4), var.get(b));						
						
						b.newAsyncChild(new AbstractTask("c") {
							@Override public void run(final Interval c) {
								Assert.assertEquals(Integer.valueOf(4), var.get());
								var.set(c, 5);
								Assert.assertEquals(Integer.valueOf(3), var.get(a));
								Assert.assertEquals(Integer.valueOf(4), var.get(b));
								Assert.assertEquals(Integer.valueOf(5), var.get(c));
								Assert.assertEquals(Integer.valueOf(5), var.get());
							}
						});
					}
				});
				
				Assert.assertEquals(Integer.valueOf(3), var.get());
				Assert.assertEquals(Integer.valueOf(3), var.get(a));
			}
		});
	}
	
}
