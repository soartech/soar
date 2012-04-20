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
public class Test_Speed_Experiment {
public static int runTest(){
        Glue_Test tester=new Glue_Test("Test_RL_Episode_Experiment");


        RLGlue.RL_init();
//Run an episode to get the JVM warmed up, etc             

        int isTerminal = RLGlue.RL_episode(500);
        
        
        long startTime=System.currentTimeMillis();

        RLGlue.RL_episode(0);
        long endTime=System.currentTimeMillis();

        int steps=RLGlue.RL_num_steps();
        System.out.println("MS to run Episode Type 1 "+(endTime-startTime)+" (per step: "+((double)(endTime-startTime)/(double)steps)+")");

        startTime=System.currentTimeMillis();

        RLGlue.RL_episode(0);
        endTime=System.currentTimeMillis();

        steps=RLGlue.RL_num_steps();
        System.out.println("MS to run Episode Type 2 "+(endTime-startTime)+" (per step: "+((double)(endTime-startTime)/(double)steps)+")");

        
        System.out.println(tester);

        return tester.getFailCount();

}    
    public static void main(String[] args){
        
       System.exit(runTest());
    }

}
