/*
 * Copyright (C) 1998 by ETHZ/INF/CS
 * All rights reserved
 *
 * $Id: TaskFactory.java 96 2001-03-16 18:00:29Z praun $
 * 
 * 03/02/99  cvp
 *
 */

import java.text.*;
import java.util.*;

public class TaskFactory {
    
    private static String[] TFA_SITES_ = {"SOHO_SYNOPTIC", 
						"RAG"};
    private static MetaSearchResult[] TFA_PROTOTYPES_ = {
	new SohoSynoptic(), 
	new Rag()
    };
    
    public List makeTasks(Hashtable parameters, Date date, MetaSearchRequest r) {
	List ret = new LinkedList();
	for (int i=0; i < TFA_SITES_.length; ++i) {
	    String s = (String) parameters.get(TFA_SITES_[i]);
	    if ("1".equals(s)) {
		MetaSearchResult tmp = MetaSearchResult.cloneTask(TFA_PROTOTYPES_[i]);
		tmp.date = date;
		tmp.request = r;
		ret.add(tmp);
	    }
	}
	return ret;
    }
}
