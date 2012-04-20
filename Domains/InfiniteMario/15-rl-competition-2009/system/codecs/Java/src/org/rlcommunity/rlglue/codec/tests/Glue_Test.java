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

/**
 *
 * @author Brian Tanner
 */
public class Glue_Test {
private int tests_failed=0;
private int test_count=0;

String callerName;

public Glue_Test(String callerName){
    this.callerName=callerName;
}

public void check_fail(boolean didFail){
	test_count++;
	if(didFail){
		System.out.println("Failed check: "+test_count);
		tests_failed++;
	}
}

public String toString(){
    if(tests_failed>0)
       return "Failed "+tests_failed+" / "+test_count+" in "+callerName;

    return "Passed all "+test_count+" checks in "+callerName;
    
}

    public int getFailCount() {
       return tests_failed;
    }
	
}
