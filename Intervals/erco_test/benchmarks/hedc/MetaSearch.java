/*
 * Copyright (C) 1998 by ETHZ/INF/CS
 * All rights reserved
 * 
 * @version $Id: MetaSearch.java 3342 2003-07-31 09:36:46Z praun $
 * @author Christoph von Praun
 */

import java.util.Hashtable;
import java.util.List;

public interface MetaSearch {
    /* returns a list of MetaSearchResults */
    List search(Hashtable parameters, MetaSearchRequest r);
}
