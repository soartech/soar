/* 
* Copyright (C) 2007, Brian Tanner
* 
http://rl-glue-ext.googlecode.com/

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
* 
*  $Revision: 489 $
*  $Date: 2009-01-31 16:34:21 -0500 (Sat, 31 Jan 2009) $
*  $Author: brian@tannerpages.com $
*  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/EnvironmentInterface.java $
* 
*/


package org.rlcommunity.rlglue.codec;

import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;

/**
 * This is the interface that all environments should implement.
 * @author btanner
 */
public interface EnvironmentInterface
{
    String env_init();
    Observation env_start();
    Reward_observation_terminal env_step(Action action);
    void env_cleanup();
    String env_message(final String message);
}
