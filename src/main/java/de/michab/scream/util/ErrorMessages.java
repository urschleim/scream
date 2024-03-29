/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.smack.util.JavaUtil;
import org.smack.util.resource.ResourceMap;

/**
 * The error messages.
 */
public class ErrorMessages
{
    /**
     * The error id to message mappings.
     */
    public final static Map<String, String> map = JavaUtil.make( () -> {
        ResourceMap rm = ResourceMap.getResourceMap( ErrorMessages.class );

        if ( rm == null )
            throw new AssertionError( "No resources for " + ErrorMessages.class.getName() );

        HashMap<String, String> n = new HashMap<>();

        for ( var c : rm.keySet() )
            n.put( c, rm.get( c ) );

        return Collections.unmodifiableMap( n );
    });
}
