/*
 * Copyright (C) 1998 by ETHZ/INF/CS
 * All rights reserved
 *
 * $Id: Task.java 96 2001-03-16 18:00:29Z praun $
 * 
 * 03/02/99  cvp
 *
 */

import java.util.*;

public abstract class Task implements Cloneable, Runnable {

    /**
     * the thread that handles this task.
     */
    protected Thread thread_ = null;

    /** 
     * Item will not be processed by the Worker 
     * executor if set
     */ 
    public boolean valid = true;

    /**
     *  The request thread that indirectly issued this task
     */
    public synchronized void setThread(Thread t) {
	if (!valid)
	    thread_.interrupt();
	else
	    thread_ = t;
    }
    
    public abstract void cancel();
    
    public void run() {
	try {
	    runImpl();
	} catch(Exception e) {
	    Messages.debug(-1, "Task::run exception=%1", e);
	    // e.printStackTrace();
	}
	thread_ = null;
    }

    public abstract void runImpl() throws Exception;
}
