/**************************************************************************
 *                                                                         *
 *             Java Grande Forum Benchmark Suite - Version 2.0             *
 *                                                                         *
 *                            produced by                                  *
 *                                                                         *
 *                  Java Grande Benchmarking Project                       *
 *                                                                         *
 *                                at                                       *
 *                                                                         *
 *                Edinburgh Parallel Computing Centre                      *
 *                                                                         * 
 *                email: epcc-javagrande@epcc.ed.ac.uk                     *
 *                                                                         *
 *                  Original version of this code by                       *
 *                         Dieter Heermann                                 * 
 *                       converted to Java by                              *
 *                Lorna Smith  (l.smith@epcc.ed.ac.uk)                     *
 *                   (see copyright notice below)                          *
 *                                                                         *
 *      This version copyright (c) The University of Edinburgh, 2001.      *
 *                         All rights reserved.                            *
 *                                                                         *
 **************************************************************************/

package jgfmt.section3.moldyn;

import jgfmt.jgfutil.JGFInstrumentor;
import ch.ethz.intervals.DoubleReduction;
import ch.ethz.intervals.IndexedTask;
import ch.ethz.intervals.IntReduction;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;

public class mdIntervals extends mdBase {
	
	int lg, mdsize;

//	double l, rcoff, rcoffs, side, sideh, hsq, hsq2, velt;
//	double a, r, tscale, sc, ts;
	final double den = 0.83134;
	final double tref = 0.722;
	final double h = 0.064;
//	double vaver, vaverh, rand;
//	double u1, u2, v1, v2, s, xx, yy, zz;
//	double xvelocity, yvelocity, zvelocity;

	DoubleReduction[][] sh_force;
	
	double result;

	//int npartm, iseed, tint;
	final int irep = 10;
	final int istop = 19;
	final int iprint = 10;
	final int movemx = 50;

	//Barrier br;
	random randnum;

	particle one[] = null;

	public DoubleReduction epot, vir;
	public IntReduction interacts;

	public void runiters() {

		/* Create new arrays */

		epot = new DoubleReduction(0.0);
		vir = new DoubleReduction(0.0);
		interacts = new IntReduction(0);

		DoubleReduction sh_force[][] = new DoubleReduction[3][PARTSIZE];		
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < PARTSIZE; j++)
				sh_force[i][j] = new DoubleReduction(0.0);

		/* Setup parameters */

		mdsize = md.PARTSIZE;
		one = new particle[mdsize];
		final double l = md.LENGTH;

		final double side = Math.pow((mdsize / den), 0.3333333);
		final double rcoff = mm / 4.0;

		final double a = side / mm;
		final double sideh = side * 0.5;
		final double hsq = h * h;
		final double hsq2 = hsq * 0.5;
		final double npartm = mdsize - 1;
		final double rcoffs = rcoff * rcoff;
		final double tscale = 16.0 / (1.0 * mdsize - 1.0);
		final double vaver = 1.13 * Math.sqrt(tref / 24.0);
		final double vaverh = vaver * h;

		/* Particle Generation */

		final double xvelocity = 0.0;
		final double yvelocity = 0.0;
		final double zvelocity = 0.0;

		int ijk = 0;
		for (lg = 0; lg <= 1; lg++) {
			for (int i = 0; i < mm; i++) {
				for (int j = 0; j < mm; j++) {
					for (int k = 0; k < mm; k++) {
						one[ijk] = new particle(
								(i * a + lg * a * 0.5),
								(j * a + lg * a * 0.5),
								(k * a),
								xvelocity,
								yvelocity,
								zvelocity,
								sh_force,
								this);
						ijk = ijk + 1;
					}
				}
			}
		}
		for (lg = 1; lg <= 2; lg++) {
			for (int i = 0; i < mm; i++) {
				for (int j = 0; j < mm; j++) {
					for (int k = 0; k < mm; k++) {
						one[ijk] = new particle(
								(i * a + (2 - lg) * a * 0.5),
								(j * a + (lg - 1) * a * 0.5),
								(k * a + a * 0.5),
								xvelocity,
								yvelocity,
								zvelocity,
								sh_force,
								this);
						ijk = ijk + 1;
					}
				}
			}
		}

		/* Initialise velocities */

		final int iseed = 0;
		final double v1 = 0.0;
		final double v2 = 0.0;

		randnum = new random(iseed, v1, v2);

		for (int i = 0; i < mdsize; i += 2) {
			double r = randnum.seed();
			one[i].xvelocity = r * randnum.v1;
			one[i + 1].xvelocity = r * randnum.v2;
		}

		for (int i = 0; i < mdsize; i += 2) {
			double r = randnum.seed();
			one[i].yvelocity = r * randnum.v1;
			one[i + 1].yvelocity = r * randnum.v2;
		}

		for (int i = 0; i < mdsize; i += 2) {
			double r = randnum.seed();
			one[i].zvelocity = r * randnum.v1;
			one[i + 1].zvelocity = r * randnum.v2;
		}

		/* velocity scaling */

		double ekin = 0.0;
		double sp = 0.0;

		for (int i = 0; i < mdsize; i++) {
			sp = sp + one[i].xvelocity;
		}
		sp = sp / mdsize;

		for (int i = 0; i < mdsize; i++) {
			one[i].xvelocity = one[i].xvelocity - sp;
			ekin = ekin + one[i].xvelocity * one[i].xvelocity;
		}

		sp = 0.0;
		for (int i = 0; i < mdsize; i++) {
			sp = sp + one[i].yvelocity;
		}
		sp = sp / mdsize;

		for (int i = 0; i < mdsize; i++) {
			one[i].yvelocity = one[i].yvelocity - sp;
			ekin = ekin + one[i].yvelocity * one[i].yvelocity;
		}

		sp = 0.0;
		for (int i = 0; i < mdsize; i++) {
			sp = sp + one[i].zvelocity;
		}
		sp = sp / mdsize;

		for (int i = 0; i < mdsize; i++) {
			one[i].zvelocity = one[i].zvelocity - sp;
			ekin = ekin + one[i].zvelocity * one[i].zvelocity;
		}

		final double ts = tscale * ekin;
		double sc = h * Math.sqrt(tref / ts);

		for (int i = 0; i < mdsize; i++) {

			one[i].xvelocity = one[i].xvelocity * sc;
			one[i].yvelocity = one[i].yvelocity * sc;
			one[i].zvelocity = one[i].zvelocity * sc;

		}

		/* main calculation */
		JGFInstrumentor.startTimer("Section3:MolDyn:Run");
		for (int move = 0; move < movemx; move++) {
			/* move the particles and update velocities */
			
			// use accumulate shared force to update position of all particles
			Intervals.blockingInterval(new IndexedTask(mdsize) {
				public void run(Point _, int start, int stop) {
					for(int i = start; i < stop; i++)
						one[i].domove(side, i);
				}				
			});

			// reset accumulate shared force for all particles
			for (int j = 0; j < 3; j++) {
				for (int i = 0; i < mdsize; i++) {
					sh_force[j][i].setValue(0.0);
				}
			}

			// reset accumulate shared force for all particles
			epot.resetAccumulators();
			vir.resetAccumulators();
			interacts.resetAccumulators();

			/* compute forces */
			Intervals.blockingInterval(new IndexedTask(mdsize) {
				public void run(Point _, int start, int stop) {
					for(int i = start; i < stop; i++)
						one[i].force(side, rcoff, mdsize, i);
				}				
			});

			/* reduce accumulated calculations */
			for (int k = 0; k < 3; k++)
				for (int i = 0; i < mdsize; i++)
					sh_force[k][i].reduce();
			epot.reduce();
			vir.reduce();
			interacts.reduce();

			for (int j = 0; j < 3; j++)
				for (int i = 0; i < mdsize; i++) {
					DoubleReduction r = sh_force[j][i];
					r.setValue(r.value() * hsq2);
				}
			

			/* scale forces, update velocities */
			double sum = 0.0;
			for (int i = 0; i < mdsize; i++) {
				sum = sum + one[i].mkekin(hsq2, i);
			}

			ekin = sum / hsq;

			double vel = 0.0;
			double count = 0.0;

			/* average velocity */

			for (int i = 0; i < mdsize; i++) {
				double velt = one[i].velavg(vaverh, h);
				if (velt > vaverh) {
					count = count + 1.0;
				}
				vel = vel + velt;
			}

			vel = vel / h;

			/* temperature scale if required */

			if ((move < istop) && (((move + 1) % irep) == 0)) {
				sc = Math.sqrt(tref / (tscale * ekin));
				for (int i = 0; i < mdsize; i++) {
					one[i].dscal(sc, 1);
				}
				ekin = tref / tscale;
			}

			/* sum to get full potential energy and virial */

			if (((move + 1) % iprint) == 0) {				
				double ek = result = 24.0 * ekin;
				double epot4 = epot.value() * 4.0;
				double etot = ek + epot4;
				double temp = tscale * ekin;
				double pres = den * 16.0 * (ekin - vir.value()) / mdsize;
				vel = vel / mdsize;
				double rp = (count / mdsize) * 100.0;
			}
		}

		JGFInstrumentor.stopTimer("Section3:MolDyn:Run");
	}
	
	public void setupParameters() {
		/* Parameter determination */


	}
	
	static class particle {

		public double xcoord, ycoord, zcoord;
		public double xvelocity, yvelocity, zvelocity;
		int part_id;
		DoubleReduction[][] sh_force;
		mdIntervals runner;

		public particle(
				double xcoord,
				double ycoord,
				double zcoord,
				double xvelocity,
				double yvelocity,
				double zvelocity,
				DoubleReduction[][] sh_force,
				mdIntervals runner)
		{

			this.xcoord = xcoord;
			this.ycoord = ycoord;
			this.zcoord = zcoord;
			this.xvelocity = xvelocity;
			this.yvelocity = yvelocity;
			this.zvelocity = zvelocity;
			this.sh_force = sh_force;
			this.runner = runner;
		}

		public void domove(double side, int part_id) {

			xcoord = xcoord + xvelocity + sh_force[0][part_id].value();
			ycoord = ycoord + yvelocity + sh_force[1][part_id].value();
			zcoord = zcoord + zvelocity + sh_force[2][part_id].value();

			if (xcoord < 0) {
				xcoord = xcoord + side;
			}
			if (xcoord > side) {
				xcoord = xcoord - side;
			}
			if (ycoord < 0) {
				ycoord = ycoord + side;
			}
			if (ycoord > side) {
				ycoord = ycoord - side;
			}
			if (zcoord < 0) {
				zcoord = zcoord + side;
			}
			if (zcoord > side) {
				zcoord = zcoord - side;
			}

			xvelocity = xvelocity + sh_force[0][part_id].value();
			yvelocity = yvelocity + sh_force[1][part_id].value();
			zvelocity = zvelocity + sh_force[2][part_id].value();

		}

		public void force(
				double side,
				double rcoff,
				int mdsize,
				int x)
		{

			final double sideh = 0.5 * side;
			final double rcoffs = rcoff * rcoff;

			double fxi = 0.0;
			double fyi = 0.0;
			double fzi = 0.0;

			for (int i = x + 1; i < mdsize; i++) {
				double xx = this.xcoord - runner.one[i].xcoord;
				double yy = this.ycoord - runner.one[i].ycoord;
				double zz = this.zcoord - runner.one[i].zcoord;

				if (xx < (-sideh)) {
					xx = xx + side;
				}
				if (xx > (sideh)) {
					xx = xx - side;
				}
				if (yy < (-sideh)) {
					yy = yy + side;
				}
				if (yy > (sideh)) {
					yy = yy - side;
				}
				if (zz < (-sideh)) {
					zz = zz + side;
				}
				if (zz > (sideh)) {
					zz = zz - side;
				}

				double rd = xx * xx + yy * yy + zz * zz;

				if (rd <= rcoffs) {
					double rrd = 1.0 / rd;
					double rrd2 = rrd * rrd;
					double rrd3 = rrd2 * rrd;
					double rrd4 = rrd2 * rrd2;
					double rrd6 = rrd2 * rrd4;
					double rrd7 = rrd6 * rrd;
					runner.epot.add(rrd6 - rrd3);
					double r148 = rrd7 - 0.5 * rrd4;
					runner.vir.subtract(rd * r148);
					double forcex = xx * r148;
					fxi = fxi + forcex;

					sh_force[0][i].subtract(forcex);

					double forcey = yy * r148;
					fyi = fyi + forcey;

					sh_force[1][i].subtract(forcey);

					double forcez = zz * r148;
					fzi = fzi + forcez;

					sh_force[2][i].subtract(forcez);

					runner.interacts.add(1);
				}

			}

			sh_force[0][x].add(fxi);
			sh_force[1][x].add(fyi);
			sh_force[2][x].add(fzi);
		}

		public double mkekin(double hsq2, int part_id) {

			double sumt = 0.0;

			xvelocity = xvelocity + sh_force[0][part_id].value();
			yvelocity = yvelocity + sh_force[1][part_id].value();
			zvelocity = zvelocity + sh_force[2][part_id].value();

			sumt = (xvelocity * xvelocity) + (yvelocity * yvelocity)
					+ (zvelocity * zvelocity);
			return sumt;
		}

		public double velavg(double vaverh, double h) {

			double velt;
			double sq;

			sq = Math.sqrt(xvelocity * xvelocity + yvelocity * yvelocity
					+ zvelocity * zvelocity);

			velt = sq;
			return velt;
		}

		public void dscal(double sc, int incx) {

			xvelocity = xvelocity * sc;
			yvelocity = yvelocity * sc;
			zvelocity = zvelocity * sc;

		}

	}

	@Override
	public double getinteractions() {
		return interacts.value();
	}

	@Override
	public double getresult() {
		return result;
	}


}

