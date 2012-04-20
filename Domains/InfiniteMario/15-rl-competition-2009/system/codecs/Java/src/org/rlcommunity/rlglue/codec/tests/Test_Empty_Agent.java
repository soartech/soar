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

import org.rlcommunity.rlglue.codec.AgentInterface;
import org.rlcommunity.rlglue.codec.util.AgentLoader;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;

/**
 *
 * @author Brian Tanner
 */
public class Test_Empty_Agent implements AgentInterface {

    int whichEpisode = 0;
    Action emptyAction=new Action(0,0,0);
    Action nonEmptyAction=new Action(7,3,1);

    public Test_Empty_Agent() {
    }

    public void agent_init(String taskSpecString) {
        TestUtility.set_k_ints_in_abstract_type(nonEmptyAction, 7);
        TestUtility.set_k_doubles_in_abstract_type(nonEmptyAction, 3);
        TestUtility.set_k_chars_in_abstract_type(nonEmptyAction, 1);
        whichEpisode=0;
    }

    public Action agent_start(Observation o) {
        whichEpisode++;
        
        if(whichEpisode%2==0)
            return emptyAction;
                    
        return nonEmptyAction;
    }

    public Action agent_step(double arg0, Observation o) {
        if(whichEpisode%2==0)
            return emptyAction;
                    
        return nonEmptyAction;
    }

    public void agent_end(double arg0) {
        // TODO Auto-generated method stub
    }

    public String agent_message(String inMessage) {
        return "";
    }

    public void agent_cleanup() {
        // TODO Auto-generated method stub
    }
    
    public static void main(String[] args){
        AgentLoader L=new AgentLoader(new Test_Empty_Agent());
        L.run();
    }
}
