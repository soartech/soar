/*
 * Copyright 2008 Brian Tanner
 * http://rl-glue-ext.ext.googlecode.com/
 * brian@tannerpages.com
 * http://brian.tannerpages.com
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.rlcommunity.rlglue.codec.tests;

import org.rlcommunity.rlglue.codec.types.RL_abstract_type;

/**
     * Ported from the RL-Glue C tests.  So much nicer in Java.
 * @author Brian Tanner
 */
public class TestUtility {
    /**
     * @param the_struct
     * @param num_ints
     */
public static void set_k_ints_in_abstract_type(RL_abstract_type the_struct, int num_ints){
        the_struct.intArray=new int[num_ints];
	for(int i=0;i<num_ints;i++) the_struct.intArray[i]=i;
}
public static void set_k_doubles_in_abstract_type(RL_abstract_type the_struct, int num_doubles){
        the_struct.doubleArray=new double[num_doubles];
	for(int i=0;i<num_doubles;i++) the_struct.doubleArray[i]=(double)i/(double)num_doubles;
}
public static void set_k_chars_in_abstract_type(RL_abstract_type the_struct, int num_chars){
        the_struct.charArray=new char[num_chars];
	for(int i=0;i<num_chars;i++)
            the_struct.charArray[i]=(char)(((int)'a')+i);
}

public static void clean_abstract_type(RL_abstract_type the_struct){
    the_struct.intArray=new int[0];
    the_struct.doubleArray=new double[0];
    the_struct.charArray=new char[0];
}
}
