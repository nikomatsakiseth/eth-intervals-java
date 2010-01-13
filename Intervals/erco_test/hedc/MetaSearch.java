/*
 * Copyright (C) 1998 by ETHZ/INF/CS
 * All rights reserved
 * 
 * $Id: MetaSearch.java 96 2001-03-16 18:00:29Z praun $
 * 
 * 03/06/99  cvp
 *
 */

import java.util.Hashtable;
import java.util.List;

public interface MetaSearch {
    /* returns a list of MetaSearchResults */
    List search(Hashtable parameters, MetaSearchRequest r);
}
