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
public class Test_Empty_Environment implements EnvironmentInterface {
    int whichEpisode = 0;
    
    Observation emptyObservation=new Observation(0,0,0);
    Observation nonEmptyObservation=new Observation(2,4,5);

    
    
    public Test_Empty_Environment() {
    }

    
    public String env_message(String inMessage) {
        return "";
    }

    public static void main(String[] args){
        EnvironmentLoader L=new EnvironmentLoader(new Test_Empty_Environment());
        L.run();
    }

    public String env_init() {
	whichEpisode=0;

        TestUtility.clean_abstract_type(emptyObservation);
        TestUtility.clean_abstract_type(nonEmptyObservation);

        TestUtility.set_k_ints_in_abstract_type(nonEmptyObservation, 2);
        TestUtility.set_k_doubles_in_abstract_type(nonEmptyObservation, 4);
        TestUtility.set_k_chars_in_abstract_type(nonEmptyObservation, 5);

	return "";
    }
    public Observation env_start() {
	whichEpisode++;
	
	if(whichEpisode%2==0)
		return emptyObservation;

	return nonEmptyObservation;
    }

    public Reward_observation_terminal env_step(Action action) {
        
        Reward_observation_terminal ro=new Reward_observation_terminal();
        
	if(whichEpisode%2==0)
            ro.o=emptyObservation;
        else
            ro.o=nonEmptyObservation;
        return ro;
    }

    public void env_cleanup() {
    }

}
