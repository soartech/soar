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
public class Test_1_Environment implements EnvironmentInterface {

    int stepCount = 0;
    Observation o = new Observation();

    public Test_1_Environment() {
    }

    public String env_message(String inMessage) {
        int timesToPrint = stepCount % 3;
        StringBuffer b = new StringBuffer();

        b.append(inMessage);
        b.append("|");
        for (int i = 0; i < timesToPrint; i++) {
            b.append(stepCount);
            b.append(".");
        }
        b.append("|");
        b.append(inMessage);
        return b.toString();
    }

    public String env_init() {
        return "sample task spec";
    }

    public Observation env_start() {
        stepCount = 0;
        TestUtility.clean_abstract_type(o);
        TestUtility.set_k_ints_in_abstract_type(o, 1);
        TestUtility.set_k_doubles_in_abstract_type(o, 2);
        TestUtility.set_k_chars_in_abstract_type(o, 3);
        return o;
    }

    public Reward_observation_terminal env_step(Action action) {
        TestUtility.clean_abstract_type(o);
        Reward_observation_terminal ro = null;
        /* Gabor has made it so this environment will step past terminal.  This is
        not something we want to do in general at all.

        But, in order to keep the other tests all working, I'll allow it*/
        if (5 > stepCount) {

            TestUtility.set_k_ints_in_abstract_type(o, 1);
            o.intArray[0] = stepCount;
            stepCount++;

            boolean terminal = stepCount == 5;
            ro = new Reward_observation_terminal(1.0d, o, terminal);
        } else {
            TestUtility.set_k_ints_in_abstract_type(o, 5);
            TestUtility.set_k_doubles_in_abstract_type(o, 5);
            TestUtility.set_k_chars_in_abstract_type(o, 5);


            o.intArray[0] = 173;
            o.intArray[1] = -173;
            o.intArray[2] = 2147483647;
            o.intArray[3] = 0;
            o.intArray[4] = -2147483648;

            o.doubleArray[0] = 0.0078125;
            o.doubleArray[1] = -0.0078125;
            o.doubleArray[2] = 0.0;
            o.doubleArray[3] = 0.0078125e150;
            o.doubleArray[4] = -0.0078125e150;

            o.charArray[0] = 'g';
            o.charArray[1] = 'F';
            o.charArray[2] = '?';
            o.charArray[3] = ' ';
            o.charArray[4] = '&';

            ro = new Reward_observation_terminal(-2.0d, o, false);
        }
        return ro;
    }

    public void env_cleanup() {
    }

    public static void main(String[] args) {
        EnvironmentLoader L = new EnvironmentLoader(new Test_1_Environment());
        L.run();
    }
}
