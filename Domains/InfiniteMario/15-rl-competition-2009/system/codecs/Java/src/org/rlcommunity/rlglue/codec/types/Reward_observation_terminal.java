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
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/types/Reward_observation_terminal.java $
 *
 */
package org.rlcommunity.rlglue.codec.types;

/**
 * I'm iteratively updating these methods to have getters and setters for
 * all components because eventually we might like to make the actual members
 * private.
 * @author btanner
 */
public class Reward_observation_terminal {

    public double r;
    public Observation o;
    public int terminal;

    public Reward_observation_terminal() {
        r = 0;
        o = new Observation();
        terminal = 0;
    }

    public Reward_observation_terminal(double reward, Observation observation, int terminal) {
        this(reward, observation, terminal == 1);
    }

    /**
     * Set the reward value.
     * @since 2.0
     * @param newReward
     */
    public void setReward(double newReward) {
        this.r = newReward;
    }

    public double getReward() {
        return r;
    }

    /**
     * @since 2.0 Want to move towards using terminal as a boolean not an int.
     */
    public Reward_observation_terminal(double reward, Observation observation, boolean terminal) {
        this.r = reward;
        this.o = observation;
        if (terminal) {
            this.terminal = 1;
        } else {
            this.terminal = 0;
        }
    }

    /**
     * This is a deep copy constructor
     * @param src
     */
    public Reward_observation_terminal(Reward_observation_terminal src) {
        this.o = src.o.duplicate();
        this.r = src.r;
        this.terminal = src.terminal;
    }

    /**
     * @since 2.0 Want to move towards using terminal as a boolean not an int.
     * @return boolean representation of whether the episode is over.
     */
    public boolean isTerminal() {
        return terminal == 1;
    }

    /**
     * One day we will make the members private and you'll have to use accessors.
     * It would be better if you used the version that returns a boolean, but this  is
     * better than accessing the members directly.
     * @return an integer, 1 if terminal, 0 if not
     * @deprecated  use isTerminal
     */
    public int getTerminal() {
        return terminal;
    }

    /**
     * Adding this method in an effort to get us away from the integer terminal
     * type.  Eventually we can just use the methods and privatize the members.
     * @since 2.0
     * @param newTerminal
     * @return
     */
    public void setTerminal(boolean newTerminal) {
        if (newTerminal) {
            this.terminal = 1;
        } else {
            this.terminal = 0;
        }
    }

    /**
     * @since 2.0
     * @param o
     */
    public void setObservation(Observation o) {
        this.o = o;
    }

    public Observation getObservation() {
        return o;
    }

    public Reward_observation_terminal duplicate() {
        return new Reward_observation_terminal(this);
    }
}
