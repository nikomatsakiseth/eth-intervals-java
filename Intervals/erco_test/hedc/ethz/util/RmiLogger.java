/*
 * Copyright (C) 1998 by ETHZ/INF/CS
 * All rights reserved
 * 
 * $Id: RmiLogger.java 97 2001-03-16 18:17:54Z praun $
 * 
 * 28/03/99  cvp 
 *
 */
 
package ethz.util;

import java.io.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;

/*
 *
 */
public class RmiLogger extends Logger {
    
    private static RmiLogger uniqueInstance_ = null;
    private static final String LOG_ID_ = "$Id: RmiLogger.java 97 2001-03-16 18:17:54Z praun $";
    private static final String LOG02_ = "LOG02 - error occurred during logging (%1)";

    private RmiLogger() {
	SystemProperties op = SystemProperties.getUniqueInstance();
	String filename = op.getString("ethz.util.RmiLogger.FILE", null);
	int bufsize = op.getInteger("ethz.util.HttpLogger.BUF_SIZE", -1);
	openLog(filename, bufsize);
    }

    protected synchronized void writePreamble_() {
	if (log_ != null) {
	    try {
		log_.writeBytes("#Version: 1.0\n");
		log_.writeBytes("#Start-Date: " + (new Date()).toString() + "\n");
		log_.writeBytes("#Software: ethz.util.RmiLogger\n");
		log_.writeBytes("#Fields: c-dns c-user s-user [s-date s-time] cs-method {cs-parameters} cs-version s-exception cs-return s-time-taken\n");
	    } catch (Exception e) {
		Messages.error(LOG02_, e.getMessage());
		log_ = null;
	    }
	}
    }

    public void log(String procedure, Hashtable parameters, String user, String remoteHost, String exception, Object retVal, long duration) {
	try {
	    Calendar cal = Calendar.getInstance(); 
	    int date = cal.get(Calendar.DAY_OF_MONTH);
	    int hours = cal.get(Calendar.HOUR_OF_DAY);
	    int seconds = cal.get(Calendar.SECOND);
	    int month = cal.get(Calendar.MONTH);
	    int year = cal.get(Calendar.YEAR);
	    int minutes = cal.get(Calendar.MINUTE);
	    int timeZoneOffset = cal.get(Calendar.ZONE_OFFSET) / 3600000 + cal.get(Calendar.DST_OFFSET) / 3600000;
	    
	    buffer_.append((remoteHost == null) ? "-" : remoteHost);
	    buffer_.append(" - " + ((user == null) ? "-" : user));			  	   // user name
	    buffer_.append(((date < 10) ? " [0" : " [")
			   + (date 		   	   // current date
			      + "/" + monthnames[month]
			      + "/" + (year)
			      + ((hours < 10)
				 ? (":0" + hours)
			    : (":" + hours))
			      + ((minutes < 10)
				 ? (":0" + minutes)
			    : (":" + minutes))
			      + ((seconds < 10)
				 ? (":0" + seconds)
				 : (":" + seconds))
			      + ((timeZoneOffset < 0)
				 ? " " + (timeZoneOffset)
				 : " +" + (timeZoneOffset))
			      + "]")
			   ); // current date
	    buffer_.append(" " + procedure + " " + parameters +  " " + "RMI"
			   + " " +  ((exception == null) ? "-" : exception)
			   + " " + ((retVal == null) ? "-" : "" + retVal) 
			   + " " + ((duration == 0) ? "-" : "" + duration)        // duration of this request in ms
			   + "\n" );
	    
	    // write it to permanent storage if necessary
	    appendLogBuffer();
	} catch (Throwable e) {
	    e.printStackTrace();
	    Messages.warn(0, LOG02_, e.getMessage());    
	}
    }

    /**
     * Singleton pattern: returns the unique instance of this class.
     * Lazy initialization here.
     *
     * @return The unique instance.
     */
    public static RmiLogger getUniqueInstance() {
	if (uniqueInstance_ == null) {
	    uniqueInstance_ = new RmiLogger();
	}
	return uniqueInstance_;
    }
    
    protected void writeEpilogue_() {
		if (log_ != null) {
	    try {
		log_.writeBytes("#End-Date: " + (new Date()).toString() + "\n");
	    } catch (Exception e) {
		Messages.error(LOG02_, e.getMessage());
		log_ = null;
	    }
	}
    }

}

