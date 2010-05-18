package ch.ethz.intervals.impl;

import static org.junit.Assert.assertFalse;
import jgfmt.section2.crypt.JGFCryptBench;
import jgfmt.section2.lufact.JGFLUFactBench;
import jgfmt.section2.series.JGFSeriesBench;
import jgfmt.section2.sor.JGFSORBench;
import jgfmt.section2.sor.SOR;
import jgfmt.section3.moldyn.JGFMolDynBench;
import jgfmt.section3.montecarlo.JGFMonteCarloBench;
import jgfmt.section3.raytracer.JGFRayTracerBench;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class TestJGFMT {
	
	@Before public void ClearGTotal() {
		SOR.Gtotal = 0.0; // benchmark uses static data to transmit result, not sure why...
	}
	
	@Test public void CryptA() {
		JGFCryptBench cb = new JGFCryptBench(-1);
		cb.JGFrun(0);
		assertFalse(cb.failed);
	}	

	@Test public void CryptB() {
		JGFCryptBench cb = new JGFCryptBench(-1);
		cb.JGFrun(1);
		assertFalse(cb.failed);
	}		
	
	@Ignore("Slow") 
	@Test public void CryptC() {
		JGFCryptBench cb = new JGFCryptBench(-1);
		cb.JGFrun(2);
		assertFalse(cb.failed);
	}		
	
	@Test public void LUFactA() {
		JGFLUFactBench lub = new JGFLUFactBench(-1);
		lub.JGFrun(0);
		assertFalse(lub.failed);
	}		
	
	@Test public void LUFactB() {
		JGFLUFactBench lub = new JGFLUFactBench(-1);
		lub.JGFrun(1);
		assertFalse(lub.failed);
	}		
	
	@Ignore("Slow") 
	@Test public void LUFactC() {
		JGFLUFactBench lub = new JGFLUFactBench(-1);
		lub.JGFrun(2);
		assertFalse(lub.failed);
	}		
	
	@Test public void SeriesA() {
		JGFSeriesBench se = new JGFSeriesBench(-1);
		se.JGFrun(0);
		assertFalse(se.failed);
	}
	
	@Ignore("Slow") 
	@Test public void SeriesB() {
		JGFSeriesBench se = new JGFSeriesBench(-1);
		se.JGFrun(1);
		assertFalse(se.failed);
	}
	
	@Ignore("Slow") 
	@Test public void SeriesC() {
		JGFSeriesBench se = new JGFSeriesBench(-1);
		se.JGFrun(2);
		assertFalse(se.failed);
	}
	
	@Test public void SORA() {
		JGFSORBench sor = new JGFSORBench(-1);
		sor.JGFrun(0);
		assertFalse(sor.failed);
	}

	@Test public void SORB() {
		JGFSORBench sor = new JGFSORBench(-1);
		sor.JGFrun(1);
		assertFalse(sor.failed);
	}

	@Ignore("Slow") @Test public void SORC() {
		JGFSORBench sor = new JGFSORBench(-1);
		sor.JGFrun(2);
		assertFalse(sor.failed);
	}
	
	@Test public void MolDynA() {
		JGFMolDynBench mold = new JGFMolDynBench(-1);
		mold.JGFrun(0);
		assertFalse(mold.failed);
	}
	
	@Ignore("Slow") @Test public void MolDynB() {
		JGFMolDynBench mold = new JGFMolDynBench(-1);
		mold.JGFrun(1);
		assertFalse(mold.failed);
	}
	
	@Test public void MonteCarloA() {
		JGFMonteCarloBench mc = new JGFMonteCarloBench(-1);
		mc.JGFrun(0);
		assertFalse(mc.failed);
	}
	
	@Ignore("Slow") @Test public void MonteCarloB() {
		JGFMonteCarloBench mc = new JGFMonteCarloBench(-1);
		mc.JGFrun(1);
		assertFalse(mc.failed);
	}	
	
	@Test public void RayTracerA() {
		JGFRayTracerBench rtb = new JGFRayTracerBench(-1);
		rtb.JGFrun(0);
		assertFalse(rtb.failed);
	}	
	
	@Ignore("Slow") @Test public void RayTracerB() {
		JGFRayTracerBench rtb = new JGFRayTracerBench(-1);
		rtb.JGFrun(1);
		assertFalse(rtb.failed);
	}	
}
