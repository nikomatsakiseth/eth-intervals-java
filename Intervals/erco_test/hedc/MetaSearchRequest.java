/*
 * Copyright (C) 1998 by ETHZ/INF/CS
 * All rights reserved
 * 
 * $Id: MetaSearchRequest.java 96 2001-03-16 18:00:29Z praun $
 * 
 * 03/02/99  cvp
 * 29/03/99  synchronize the logger when the servlet is destroyed
 *           cvp
 *
 */


import java.util.*;
import java.io.*;

public class MetaSearchRequest {
    
    private long size_ = -1;
    private Writer wrt_ = null;
    private Hashtable params_ = null;
    private MetaSearchImpl msi_ = null;
    public List results = null;
    private int counter_ = 0;
    private Thread thread_ = null;

    public MetaSearchRequest(Writer w, MetaSearchImpl msi, Hashtable params) {
	wrt_ = w;
	msi_ = msi;
	params_ = params;
    }

    public void registerInterrupt(Thread t, int ctr) {
	counter_ = ctr;
	thread_ = t;
    }
    
    public synchronized void countDownInterrupt() {
	if (thread_ != null && --counter_ == 0)
	    thread_.interrupt();
    }

    public void go() throws Exception {
	if (wrt_ != null) 
	    size_ = msi_.search(params_, wrt_, this);
	else 
	    results = msi_.search(params_, this);
    }
    
    public String printResults() {
	String ret;
	if (results != null) {
	    StringBuffer sb = new StringBuffer();
	    sb.append("[");
	    for (Iterator it = results.iterator(); it.hasNext(); ) {
		sb.append(it.next());
		if (it.hasNext())
		    sb.append(",");
	    }
	    sb.append("]");
	    ret = sb.toString();
	} else
	    ret = "none";
	return ret;
    }
  
}





