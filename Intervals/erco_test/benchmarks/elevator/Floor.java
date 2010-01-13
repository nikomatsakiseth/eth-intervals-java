/*
 * Copyright (C) 2000 by ETHZ/INF/CS
 * All rights reserved
 * 
 * @version $Id: Floor.java 2094 2003-01-30 09:41:18Z praun $
 * @author Roger Karrer
 */

import java.lang.*;
import java.util.*;
import java.io.*;

// class to represent a floor in the control object
class Floor {

	// Lists of people waiting to go up, and down
	// The Vectors will have instances of Integer objects. The Integer will
	// store the floor that the person wants to go to
	public Vector upPeople, downPeople;

	// True if an elevator has claimed the the up or down call
	public boolean upFlag, downFlag;

	public Floor() {
		upPeople = new Vector();
		downPeople = new Vector();
		upFlag = false;
		downFlag = false;
	}
}
