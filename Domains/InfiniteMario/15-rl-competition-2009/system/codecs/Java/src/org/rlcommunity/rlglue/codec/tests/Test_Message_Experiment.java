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
public class Test_Message_Experiment {
    public static int runTest(){
           Glue_Test tester=new Glue_Test("Test_Message_Experiment");

        RLGlue.RL_init();
        
       	tester.check_fail(!"empty".equals(RLGlue.RL_env_message(null)));

	tester.check_fail(!"empty".equals(RLGlue.RL_env_message("")));

	tester.check_fail(!"empty".equals(RLGlue.RL_agent_message(null)));

	tester.check_fail(!"empty".equals(RLGlue.RL_agent_message("")));

	tester.check_fail(!"".equals(RLGlue.RL_env_message("empty")));

	tester.check_fail(!"".equals(RLGlue.RL_agent_message("empty")));

	String theResponse=RLGlue.RL_env_message("null");
	tester.check_fail(!(theResponse!=null || !"".equals(theResponse)));
	
	theResponse=RLGlue.RL_agent_message("null");
	tester.check_fail(!(theResponse!=null || !"".equals(theResponse)));


	tester.check_fail(!"1".equals(RLGlue.RL_env_message("1")));
	tester.check_fail(!"1".equals(RLGlue.RL_agent_message("1")));

	tester.check_fail(!"1000000000000000000000".equals(RLGlue.RL_env_message("1000000000000000000000")));
	tester.check_fail(!"1000000000000000000000".equals(RLGlue.RL_agent_message("1000000000000000000000")));

	tester.check_fail(!"21111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111113".equals(RLGlue.RL_env_message("21111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111113")));
	tester.check_fail(!"45555555555555555555555555555555555555555555555555555555655555555555555555555555555555555555555555555555555555559".equals(RLGlue.RL_agent_message("45555555555555555555555555555555555555555555555555555555655555555555555555555555555555555555555555555555555555559")));


        
        System.out.println(tester);

        return tester.getFailCount();
    }
    public static void main(String[] args){
     System.exit(runTest());
    }

}
