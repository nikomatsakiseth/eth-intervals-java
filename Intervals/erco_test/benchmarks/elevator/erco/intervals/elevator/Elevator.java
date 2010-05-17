package erco.intervals.elevator;
/*
 * Copyright (C) 2000 by ETHZ/INF/CS
 * All rights reserved
 * 
 * @version $Id: Elevator.java 2094 2003-01-30 09:41:18Z praun $
 * @author Roger Karrer
 */

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.Vector;

import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.VoidInlineTask;
import ch.ethz.intervals.impl.IntervalImpl;

public class Elevator {

	// shared control object
	private final Controls controls;
	private final Vector<ButtonPress> events;
	private final int numFloors, numLifts;

	// Initializer for main class, reads the input and initializes
	// the events Vector with ButtonPress objects
	private Elevator(String configFile) {
		events = new Vector<ButtonPress>();

		int numFloors = 0, numLifts = 0;
		try {
			FileReader reader = new FileReader(new File(configFile));
			StreamTokenizer st = new StreamTokenizer(reader);
			st.lowerCaseMode(true);
			st.parseNumbers();

			numFloors = readNum(st);
			numLifts = readNum(st);

			int time = 0, to = 0, from = 0;
			do {
				time = readNum(st);
				if (time != 0) {
					from = readNum(st);
					to = readNum(st);
					events.addElement(new ButtonPress(time, from, to));
				}
			} while (time != 0);
		} catch (IOException e) {
			System.err.println("error reading input: " + e.getMessage());
			e.printStackTrace(System.err);
			System.exit(1);
		}

		// Create the shared control object
		controls = new Controls(numFloors);
		this.numFloors = numFloors;
		this.numLifts = numLifts;
	}

	// Press the buttons at the correct time
	private void begin() {
		Intervals.inline(new VoidInlineTask() {			
			@Override public void run(final IntervalImpl intervalImpl) {
				// Create the elevators
				for (int i = 0; i < numLifts; i++) {
					Lift lift = new Lift(numFloors, controls);
					lift.start(intervalImpl);
				}
				
				// Create interval which will press the buttons:
				new IntervalImpl(intervalImpl) {					
					@Override protected void run() {						
						// First tick is 1
						int time = 1;

						for (int i = 0; i < events.size();) {
							ButtonPress bp = (ButtonPress) events.elementAt(i);
							// if the current tick matches the time of th next event
							// push the correct buttton
							if (time == bp.time) {
								System.out
										.println("Elevator::begin - its time to press a button");
								if (bp.onFloor > bp.toFloor)
									controls.pushDown(bp.onFloor, bp.toFloor);
								else
									controls.pushUp(bp.onFloor, bp.toFloor);
								i += 1;
							}
							// wait 1/2 second to next tick
							try {
								Thread.sleep(500);
							} catch (InterruptedException e) {
							}
							time += 1;
						}
						
						controls.setTerminated();
					}
				};
			}
		});
	}

	private int readNum(StreamTokenizer st) throws IOException {
		int tokenType = st.nextToken();

		if (tokenType != StreamTokenizer.TT_NUMBER)
			throw new IOException("Number expected!");
		return (int) st.nval;
	}

	public static void main(String args[]) {
		Elevator building = new Elevator(args[0]);
		building.begin();
	}
}
