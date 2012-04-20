/* 
 * Copyright (C) 2007, Brian Tanner
 *
http://rl-glue-ext.googlecode.com/

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 *
 *  $Revision: 489 $
 *  $Date: 2009-01-31 16:34:21 -0500 (Sat, 31 Jan 2009) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/types/RL_abstract_type.java $
 *
 */
package org.rlcommunity.rlglue.codec.types;

/**
 * Common superclass for all of the Java RL-Glue types.
 * Try to keep handles to the objects and not their arrays, 
 * because there is no guarantee that the arrays will not be 
 * reallocated during certain operations.
 * @author btanner
 */
public class RL_abstract_type implements Comparable {

    public int[] intArray = null;
    public double[] doubleArray = null;
    public char[] charArray = null;

    /**
     * Create a RL_abstract_type with arrays allocated according to numInts,
     * numDoubles, and numChars
     * @param numInts  Size of int array to allocate.
     * @param numDoubles Size of double array to allocate.
     * @param numChars Size of char array to allocate.
     */
    public RL_abstract_type(int numInts, int numDoubles, int numChars) {
        intArray = new int[numInts];
        doubleArray = new double[numDoubles];
        charArray = new char[numChars];
    }

    /**
     * Create a new RL_abstract_type that is a deep, independent copy of src.
     * @param src
     */
    public RL_abstract_type(RL_abstract_type src) {
        this(src.intArray.length, src.doubleArray.length, src.charArray.length);
        RLStructCopy(src, this);
    }

    public int getInt(int which) {
        return this.intArray[which];
    }

    public double getDouble(int which) {
        return this.doubleArray[which];
    }

    public char getChar(int which) {
        return this.charArray[which];
    }

    public void setInt(int which, int value) {
        assert (intArray != null && which < intArray.length) : "Tried to set int: " + which + " but intArray only has length " + intArray.length + ".";
        intArray[which] = value;
    }

    public void setDouble(int which, double value) {
        assert (doubleArray != null && which < doubleArray.length) : "Tried to set double: " + which + " but doubleArray only has length " + doubleArray.length + ".";
        doubleArray[which] = value;
    }

    public void setChar(int which, char value) {
        assert (charArray != null && which < charArray.length) : "Tried to set char: " + which + " but charArray only has length " + charArray.length + ".";
        charArray[which] = value;
    }

    public int getNumInts() {
        if (intArray == null) {
            intArray = new int[0];
        }
        return intArray.length;
    }

    public int getNumDoubles() {
        if (doubleArray == null) {
            doubleArray = new double[0];
        }
        return doubleArray.length;
    }

    public int getNumChars() {
        if (charArray == null) {
            charArray = new char[0];
        }
        return charArray.length;
    }

    /**
     * Useful (maybe?) utility method for deep copying one RL_Abstract_type
     * into another.
     * @param src
     * @param dest
     */
    public static void RLStructCopy(RL_abstract_type src, RL_abstract_type dest) {
        if (src == null || dest == null) {
            throw new IllegalArgumentException("Either src or dest RL_Abstract_Type was null in RLStructCopy");
        }
        if (dest.intArray.length != src.intArray.length) {
            dest.intArray = new int[src.intArray.length];
        }
        if (dest.doubleArray.length != src.doubleArray.length) {
            dest.doubleArray = new double[src.doubleArray.length];
        }
        if (dest.charArray.length != src.charArray.length) {
            dest.charArray = new char[src.charArray.length];
        }

        System.arraycopy(src.intArray, 0, dest.intArray, 0, src.intArray.length);
        System.arraycopy(src.doubleArray, 0, dest.doubleArray, 0, src.doubleArray.length);
        System.arraycopy(src.charArray, 0, dest.charArray, 0, src.charArray.length);
    }

    /**
     * Prints out a human-readable format of the RL_abstract_type, which is
     * useful for debugging.
     */
    @Override
    public String toString() {
        StringBuffer b = new StringBuffer();
        b.append("numInts: " + intArray.length);
        b.append("\n");
        b.append("numDoubles: " + doubleArray.length);
        b.append("\n");
        b.append("numChars: " + charArray.length);
        b.append("\n");
        if (intArray != null) {
            for (int i = 0; i < intArray.length; i++) {
                b.append(" " + intArray[i]);
            }
        }
        for (int i = 0; i < doubleArray.length; i++) {
            b.append(" " + doubleArray[i]);
        }
        for (int i = 0; i < charArray.length; i++) {
            b.append(" " + charArray[i]);
        }

        return b.toString();
    }

    /**
     * Allows us to easily compare abstract types so that we can put them
     * in maps and stuff.
     * @param cObject
     * @return -1 if this is 'smaller' then cObject, +1 if this is 'bigger' than cObject, 0 if they are identical.
     *
     */
    public int compareTo(Object cObject) {
        RL_abstract_type compareO = (RL_abstract_type) cObject;

        if (intArray == null & compareO.intArray != null) {
            return -1;
        }
        if (intArray != null & compareO.intArray == null) {
            return 1;
        }

        if (intArray != null & compareO.intArray != null) {
            if (intArray.length < compareO.intArray.length) {
                return -1;
            }
            if (intArray.length > compareO.intArray.length) {
                return 1;
            }

            for (int i = 0; i < intArray.length; i++) {
                if (intArray[i] < compareO.intArray[i]) {
                    return -1;
                }
                if (intArray[i] > compareO.intArray[i]) {
                    return 1;
                }
            }
        }
        if (doubleArray == null & compareO.doubleArray != null) {
            return -1;
        }
        if (doubleArray != null & compareO.doubleArray == null) {
            return 1;
        }

        if (doubleArray != null & compareO.doubleArray != null) {
            if (doubleArray.length < compareO.doubleArray.length) {
                return -1;
            }
            if (doubleArray.length > compareO.doubleArray.length) {
                return 1;
            }

            for (int i = 0; i < doubleArray.length; i++) {
                if (doubleArray[i] < compareO.doubleArray[i]) {
                    return -1;
                }
                if (doubleArray[i] > compareO.doubleArray[i]) {
                    return 1;
                }
            }
        }

        if (charArray == null & compareO.charArray != null) {
            return -1;
        }
        if (charArray != null & compareO.charArray == null) {
            return 1;
        }

        if (charArray != null & compareO.charArray != null) {
            if (charArray.length < compareO.charArray.length) {
                return -1;
            }
            if (charArray.length > compareO.charArray.length) {
                return 1;
            }

            for (int i = 0; i < charArray.length; i++) {
                if (charArray[i] < compareO.charArray[i]) {
                    return -1;
                }
                if (charArray[i] > compareO.charArray[i]) {
                    return 1;
                }
            }
        }
        return 0;
    }
}
