/**************************************************************************
 *                                                                         *
 *         Java Grande Forum Benchmark Suite - Thread Version 1.0          *
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
 *                 Original version of this code by                        *
 *            Florian Doyon (Florian.Doyon@sophia.inria.fr)                *
 *              and  Wilfried Klauser (wklauser@acm.org)                   *
 *                                                                         *
 *      This version copyright (c) The University of Edinburgh, 2001.      *
 *                         All rights reserved.                            *
 *                                                                         *
 **************************************************************************/

package jgfmt.section3.raytracer;

import ch.ethz.intervals.Dependency;
import ch.ethz.intervals.LongReduction;
import ch.ethz.intervals.ParentForNew;

public class IntervalRayTracer {
	
	final LongReduction checksum;
	
	final Scene scene;
	
	/**
	 * Lights for the rendering scene
	 */
	final Light lights[];

	/**
	 * Objects (spheres) for the rendering scene
	 */
	final Primitive prim[];

	/**
	 * The view for the rendering scene
	 */
	final View view;

	/**
	 * Alpha channel
	 */
	static final int alpha = 255 << 24;

	/**
	 * Null vector (for speedup, instead of <code>new Vec(0,0,0)</code>
	 */
	static final Vec voidVec = new Vec();

	final int datasizes[] = { 150, 500 };

	final Vec viewVec, upVec, leftVec;
	
	final Interval interval;
	
	public IntervalRayTracer(@ParentForNew("Parent") Dependency dep, LongReduction checksum, Scene scene, Interval interval) {
		this.checksum = checksum;
		this.scene = scene;
		
		// Get the objects count
		int nLights = scene.getLights();
		int nObjects = scene.getObjects();

		lights = new Light[nLights];
		prim = new Primitive[nObjects];

		// Get the lights
		for (int l = 0; l < nLights; l++) {
			lights[l] = scene.getLight(l);
		}

		// Get the primitives
		for (int o = 0; o < nObjects; o++) {
			prim[o] = scene.getObject(o);
		}

		// Set the view
		view = scene.getView();
		
		viewVec = Vec.sub(view.at, view.from);
		viewVec.normalize();

		Vec tmpVec = new Vec(viewVec);
		tmpVec.scale(Vec.dot(view.up, viewVec));

		upVec = Vec.sub(view.up, tmpVec);
		upVec.normalize();

		leftVec = Vec.cross(view.up, viewVec);
		leftVec.normalize();

		double frustrumwidth = view.dist * Math.tan(view.angle);

		upVec.scale(-frustrumwidth);
		leftVec.scale(view.aspect * frustrumwidth);
		
		this.interval = interval;
	}

	public void run(int start, int stop) {
		for(int y = start; y < stop; y++) {
			long lineChecksum = 0;
			
			// Screen variables
			int row[] = new int[interval.width]; // * (interval.yto - interval.yfrom)];
			int pixCounter = 0; // iterator
	
			// Rendering variables
			Tracer tracer = new Tracer();
			Ray r = new Ray(view.from, voidVec);
	
			// Header for .ppm file
			// System.out.println("P3");
			// System.out.println(width + " " + height);
			// System.out.println("255");
	
			// All loops are reversed for 'speedup' (cf. thinking in java p331)
	
			// For each line
			
			double ylen = (double) (2.0 * y) / (double) interval.width - 1.0;
			// System.out.println("Doing line " + y);
			// For each pixel of the line
			for (int x = 0; x < interval.width; x++) {
				double xlen = (double) (2.0 * x) / (double) interval.width - 1.0;
				r.D = Vec.comb(xlen, leftVec, ylen, upVec);
				r.D.add(viewVec);
				r.D.normalize();
				Vec col = tracer.trace(0, 1.0, r);
				
				// computes the color of the ray
				int red = (int) (col.x * 255.0);
				if (red > 255)
					red = 255;
				int green = (int) (col.y * 255.0);
				if (green > 255)
					green = 255;
				int blue = (int) (col.z * 255.0);
				if (blue > 255)
					blue = 255;
	
				lineChecksum += red;
				lineChecksum += green;
				lineChecksum += blue;
	
				// RGB values for .ppm file
				// System.out.println(red + " " + green + " " + blue);
				// Sets the pixels
				row[pixCounter++] = alpha | (red << 16) | (green << 8) | (blue);
			} // end for (x)
			
			checksum.add(lineChecksum);
		}
	}
	
	final class Tracer {
		// State that varies by line.  Mostly this could be moved
		// to local variables, except for tRay: there is a (unintentional?) 
		// dependence between recursive calls to shade().
		final Isect inter = new Isect();
		final Vec L = new Vec();
		final Ray tRay = new Ray();		
		final Vec primVec = new Vec();

		boolean intersect(Ray r, double maxt) {
			Isect tp;
			int i, nhits;

			nhits = 0;
			inter.t = 1e9;
			for (i = 0; i < prim.length; i++) {
				// uses global temporary Prim (tp) as temp.object for speedup
				tp = prim[i].intersect(r, primVec);
				if (tp != null && tp.t < inter.t) {
					inter.t = tp.t;
					inter.prim = tp.prim;
					inter.surf = tp.surf;
					inter.enter = tp.enter;
					nhits++;
				}
			}
			return nhits > 0 ? true : false;
		}

		/**
		 * Checks if there is a shadow
		 * 
		 * @param r
		 *            The ray
		 * @return Returns 1 if there is a shadow, 0 if there isn't
		 */
		int Shadow(Ray r, double tmax) {
			if (intersect(r, tmax))
				return 0;
			return 1;
		}

		/**
		 * Return the Vector's reflection direction
		 * 
		 * @return The specular direction
		 */
		Vec SpecularDirection(Vec I, Vec N) {
			Vec r;
			r = Vec.comb(1.0 / Math.abs(Vec.dot(I, N)), I, 2.0, N);
			r.normalize();
			return r;
		}

		/**
		 * Return the Vector's transmission direction
		 */
		Vec TransDir(Surface m1, Surface m2, Vec I, Vec N) {
			double n1, n2, eta, c1, cs2;
			Vec r;
			n1 = m1 == null ? 1.0 : m1.ior;
			n2 = m2 == null ? 1.0 : m2.ior;
			eta = n1 / n2;
			c1 = -Vec.dot(I, N);
			cs2 = 1.0 - eta * eta * (1.0 - c1 * c1);
			if (cs2 < 0.0)
				return null;
			r = Vec.comb(eta, I, eta * c1 - Math.sqrt(cs2), N);
			r.normalize();
			return r;
		}

		/**
		 * Returns the shaded color
		 * 
		 * @return The color in Vec form (rgb)
		 */
		Vec shade(int level, double weight, Vec P, Vec N, Vec I, Isect hit) {
			double n1, n2, eta, c1, cs2;
			Vec r;
			Vec tcol;
			Vec R;
			double t, diff, spec;
			Surface surf;
			Vec col;
			int l;

			col = new Vec();
			surf = hit.surf;
			R = new Vec();
			if (surf.shine > 1e-6) {
				R = SpecularDirection(I, N);
			}

			// Computes the effectof each light
			for (l = 0; l < lights.length; l++) {
				L.sub2(lights[l].pos, P);
				if (Vec.dot(N, L) >= 0.0) {
					t = L.normalize();

					tRay.P = P;
					tRay.D = L;

					// Checks if there is a shadow
					if (Shadow(tRay, t) > 0) {
						diff = Vec.dot(N, L) * surf.kd * lights[l].brightness;

						col.adds(diff, surf.color);
						if (surf.shine > 1e-6) {
							spec = Vec.dot(R, L);
							if (spec > 1e-6) {
								spec = Math.pow(spec, surf.shine);
								col.x += spec;
								col.y += spec;
								col.z += spec;
							}
						}
					}
				} // if
			} // for

			tRay.P = P;
			if (surf.ks * weight > 1e-3) {
				tRay.D = SpecularDirection(I, N);
				tcol = trace(level + 1, surf.ks * weight, tRay);
				col.adds(surf.ks, tcol);
			}
			if (surf.kt * weight > 1e-3) {
				if (hit.enter > 0)
					tRay.D = TransDir(null, surf, I, N);
				else
					tRay.D = TransDir(surf, null, I, N);
				tcol = trace(level + 1, surf.kt * weight, tRay);
				col.adds(surf.kt, tcol);
			}

			// garbaging...
			tcol = null;
			surf = null;

			return col;
		}

		/**
		 * Launches a ray
		 */
		Vec trace(int level, double weight, Ray r) {
			Vec P, N;
			boolean hit;

			// Checks the recursion level
			if (level > 6) {
				return new Vec();
			}

			hit = intersect(r, 1e6);
			if (hit) {
				P = r.point(inter.t);
				N = inter.prim.normal(P);
				if (Vec.dot(r.D, N) >= 0.0) {
					N.negate();
				}
				return shade(level, weight, P, N, r.D, inter);
			}
			// no intersection --> col = 0,0,0
			return voidVec;
		}
	}
}
