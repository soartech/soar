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

import org.rlcommunity.rlglue.codec.EnvironmentInterface;
import org.rlcommunity.rlglue.codec.util.EnvironmentLoader;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;

/**
 *
 * @author Brian Tanner
 */
public class Test_Message_Environment implements EnvironmentInterface {
    Observation o =new Observation();
    
    public Test_Message_Environment() {
    }

    
    public String env_message(String inMessage) {
        if(inMessage==null)
            return "null";

       if(inMessage.equals(""))
           return "empty";
        
        if(inMessage.equals("null"))
            return null;

        if(inMessage.equals("empty"))
            return "";

        return new String(inMessage);
    }

    public static void main(String[] args){
        EnvironmentLoader L=new EnvironmentLoader(new Test_Message_Environment());
        L.run();
    }

    public String env_init() {
	return "";    }

    public Observation env_start() {
        TestUtility.clean_abstract_type(o);
        return o;   
    }

    public Reward_observation_terminal env_step(Action action) {
        TestUtility.clean_abstract_type(o);
        int terminal=0;
        Reward_observation_terminal ro=new Reward_observation_terminal(0.0d, o, terminal);
        return ro;
    }

    public void env_cleanup() {
    }

}
