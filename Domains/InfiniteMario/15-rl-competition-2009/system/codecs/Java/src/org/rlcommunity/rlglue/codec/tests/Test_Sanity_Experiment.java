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

import org.rlcommunity.rlglue.codec.RLGlue;

/**
 *
 * @author Brian Tanner
 */
public class Test_Sanity_Experiment {

    public static int runTest(){
        Glue_Test tester=new Glue_Test("Test_Sanity_Experiment");

        String task_spec=RLGlue.RL_init();
        
        tester.check_fail(!task_spec.equals("sample task spec"));

        
        System.out.println(tester);
        return tester.getFailCount();

    }
    public static void main(String[] args){
       System.exit(runTest());
    }

}
