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
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/types/Observation_action.java $
 *
 */
package org.rlcommunity.rlglue.codec.types;

public class Observation_action {

    public Observation o;
    public Action a;

    public Observation_action() {
        o = new Observation();
        a = new Action();
    }

    public Observation_action(Observation theObservation, Action theAction) {
        this.o = theObservation;
        this.a = theAction;
    }

    /**
     * This is a deep copy constructor
     * @param src
     */
    public Observation_action(Observation_action src) {
        this.o = src.o.duplicate();
        this.a = src.a.duplicate();
    }

    public Observation_action duplicate() {
        return new Observation_action(this);
    }

    /**
     * @since 2.0
     * @param newAction
     */
    public void setAction(Action newAction) {
        a = newAction;
    }

    public Action getAction() {
        return a;
    }

    /**
     * @since 2.0
     * @param newObservation
     */
    public void setObservation(Observation newObservation) {
        o = newObservation;
    }

    public Observation getObservation() {
        return o;
    }
}
