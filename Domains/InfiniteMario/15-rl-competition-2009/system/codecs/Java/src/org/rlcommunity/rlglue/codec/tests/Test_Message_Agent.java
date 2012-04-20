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
public class Test_Message_Agent implements AgentInterface {
    private final Action emptyAction;

    public Test_Message_Agent() {
        emptyAction=new Action(0,0,0);
    }

    public void agent_init(String taskSpecString) {
    }

    public Action agent_start(Observation o) {
            return emptyAction;
    }

    public Action agent_step(double arg0, Observation o) {
            return emptyAction;
    }

    public void agent_end(double arg0) {
        // TODO Auto-generated method stub
    }

    public String agent_message(String inMessage) {
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

    
    public void agent_cleanup() {
    }
    
    public static void main(String[] args){
        AgentLoader L=new AgentLoader(new Test_Message_Agent());
        L.run();
    }
}
