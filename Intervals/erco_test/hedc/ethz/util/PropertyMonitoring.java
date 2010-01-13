/*
 * (c) COPYRIGHT MIT and INRIA, 1996.
 * Copyright (C) 1998 by ETHZ/INF/CS
 * All rights reserved
 * 
 * $Id: PropertyMonitoring.java 97 2001-03-16 18:17:54Z praun $
 * 
 * 28/03/99  cvp 
 *
 */

package ethz.util;

public interface PropertyMonitoring {
    
    /**
     * The callback method, invoked when any property change occurs.
     * @param name The name of the property that changed.
     * @return A boolean, if <strong>true</strong>, accept the new property
     *    value, otherwise, reject it and reset the property to its old
     *    value.
     */
    public boolean propertyChanged (String name);
}
